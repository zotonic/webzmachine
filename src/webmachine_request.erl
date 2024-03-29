%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2009 Basho Technologies
%% Based on mochiweb_request.erl, which is Copyright 2007 Mochi Media, Inc.
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc Webmachine HTTP Request Abstraction.

-module(webmachine_request).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').

-define(WMVSN, "2.0 (Z)").

-export([get_peer/1]). % used in initialization

% actual interface for controller functions
-export([
     server_header/0,
     socket/1,
     method/1,
     version/1,
     disp_path/1,
     path/1,
     raw_path/1,
     get_req_header/2,
     req_headers/1,
     req_body/2,
     stream_req_body/2,
     headers/1,
     resp_headers/1,
     out_headers/1,
     get_out_header/2,
     has_out_header/2,
     peer/1,
     get_header_value/2,
     add_response_header/3,
     add_response_headers/2,
     remove_response_header/2,
     merge_response_headers/2,
     append_to_response_body/2,
     send_response/1,
     response_code/1,
     set_response_code/2,
     set_resp_body/2,
     response_body/1,
     has_response_body/1,
     has_resp_body/1,
     do_redirect/1,
     resp_redirect/1,
     set_metadata/3,
     get_metadata/2,
     get_path_info/1,
     get_path_info/2,
     load_dispatch_data/7,
     get_path_tokens/1,
     get_app_root/1,
     parse_cookie/1,
     get_cookie_value/2,
     parse_qs/1,
     get_qs_value/2,
     get_qs_value/3,
     range/1,
     log_data/1,
     use_sendfile/0
     ]).

-include_lib("wm_reqdata.hrl").
-include_lib("webmachine_logger.hrl").

-define(IDLE_TIMEOUT, infinity).
-define(FILE_CHUNK_LENGTH, 65536).


get_peer(ReqData) ->
    case ReqData#wm_reqdata.peer of
        undefined ->
            Socket = ReqData#wm_reqdata.socket,
            Peer = peer_from_peername(mochiweb_socket:peername(Socket), ReqData),
            NewReqData = ReqData#wm_reqdata{peer=Peer},
            {Peer, NewReqData};
        _ ->
            {ReqData#wm_reqdata.peer, ReqData}
    end.

peer_from_peername({ok, {Addr={10, _, _, _}, _Port}}, ReqData) ->  
    x_peername(inet_parse:ntoa(Addr), ReqData);
peer_from_peername({ok, {Addr={172, Second, _, _}, _Port}}, ReqData) when (Second > 15) andalso (Second < 32) ->
    x_peername(inet_parse:ntoa(Addr), ReqData);
peer_from_peername({ok, {Addr={192, 168, _, _}, _Port}}, ReqData) ->
    x_peername(inet_parse:ntoa(Addr), ReqData);
peer_from_peername({ok, {{127, 0, 0, 1}, _Port}}, ReqData) ->
    x_peername("127.0.0.1", ReqData);
peer_from_peername({ok, {Addr, _Port}}, _ReqData) ->
    inet_parse:ntoa(Addr);
peer_from_peername({error, enotconn}, ReqData) ->
    x_peername("-", ReqData).

x_peername(Default, ReqData) ->
    case get_header_value("x-forwarded-for", ReqData) of
    undefined ->
        Default;
    Hosts ->
        string:strip(lists:last(string:tokens(Hosts, ",")))
    end.


get_header_value(K, ReqData) ->
    wrq:get_req_header(K, ReqData).

get_outheader_value(K, ReqData) ->
    mochiweb_headers:get_value(K, wrq:resp_headers(ReqData)).

send_response(ReqData) ->
    {Reply, RD1} = send_response_range(ReqData#wm_reqdata.response_code, ReqData),
    NewLogData = (RD1#wm_reqdata.log_data)#wm_log_data{finish_time=os:timestamp()},
    {Reply, RD1#wm_reqdata{log_data=NewLogData}}.

send_response_range(200, ReqData) ->
    {Range, RangeRD} = get_range(ReqData),
    case Range of
        X when X =:= undefined; X =:= fail ->
            send_response_code(200, all, RangeRD);
        Ranges ->
            case get_resp_body_size(wrq:resp_body(RangeRD)) of
                {ok, Size, Body1} ->
                    RangeRD1 = wrq:set_resp_body(Body1, RangeRD),
                    case range_parts(Ranges, Size) of
                        [] ->
                            %% no valid ranges
                            %% could be 416, for now we'll just return 200 and the whole body
                            send_response_code(200, all, RangeRD1);
                        PartList ->
                            % error_logger:info_msg("PARTS ~p (~p)", [PartList, wrq:path(ReqData)]), 
                            ContentType = get_outheader_value("content-type", RangeRD1), 
                            {RangeHeaders, Boundary} = get_range_headers(PartList, Size, ContentType),
                            RespHdrsRD = wrq:set_resp_headers(RangeHeaders, RangeRD1),
                            send_response_code(206, {PartList, Size, Boundary, ContentType}, RespHdrsRD)
                    end;
                {error, nosize} ->
                    send_response_code(200, all, RangeRD)
            end
    end;
send_response_range(Code, ReqData) ->
    send_response_code(Code, all, ReqData).

send_response_code(Code, Parts, ReqData) ->
    ReqData1 = wrq:set_response_code(Code, ReqData),
    LogData = (ReqData1#wm_reqdata.log_data)#wm_log_data{response_code=Code, response_length=0},
    send_response_bodyfun(wrq:resp_body(ReqData1), Code, Parts, ReqData, LogData).


send_response_bodyfun({device, IO}, Code, Parts, ReqData, LogData) ->
    Length = iodevice_size(IO),
    send_response_bodyfun({device, Length, IO}, Code, Parts, ReqData, LogData);
send_response_bodyfun({device, Length, IO}, Code, all, ReqData, LogData) ->
    Writer = fun() -> 
                Bytes = send_device_body(ReqData#wm_reqdata.socket, Length, IO),
                _ = file:close(IO),
                Bytes
             end,
    send_response_headers(Code, Length, undefined, Writer, ReqData, LogData);
send_response_bodyfun({device, _Length, IO}, Code, Parts, ReqData, LogData) ->
    Writer = fun() -> 
                Bytes = send_device_body_parts(ReqData#wm_reqdata.socket, Parts, IO),
                _ = file:close(IO),
                Bytes
             end,
    send_response_headers(Code, undefined, undefined, Writer, ReqData, LogData);
send_response_bodyfun({file, Filename}, Code, Parts, ReqData, LogData) ->
    Length = filelib:file_size(Filename),
    send_response_bodyfun({file, Length, Filename}, Code, Parts, ReqData, LogData);
send_response_bodyfun({file, Length, Filename}, Code, all, ReqData, LogData) ->
    Writer = fun() -> send_file_body(ReqData#wm_reqdata.socket, false, Length, Filename) end,
    send_response_headers(Code, Length, undefined, Writer, ReqData, LogData);
send_response_bodyfun({file, _Length, Filename}, Code, Parts, ReqData, LogData) ->
    Writer = fun() -> send_file_body_parts(ReqData#wm_reqdata.socket, Parts, Filename) end,
    send_response_headers(Code, undefined, undefined, Writer, ReqData, LogData);
send_response_bodyfun({stream, StreamFun}, Code, all, ReqData, LogData) ->
    Writer = fun() -> send_stream_body(ReqData#wm_reqdata.socket, is_chunked_transfer(wrq:version(ReqData), chunked), StreamFun) end,
    send_response_headers(Code, undefined, chunked, Writer, ReqData, LogData);
send_response_bodyfun({stream, Size, Fun}, Code, all, ReqData, LogData) ->
    Writer = fun() -> send_stream_body(ReqData#wm_reqdata.socket, is_chunked_transfer(wrq:version(ReqData), chunked), Fun(0, Size-1)) end,
    send_response_headers(Code, undefined, chunked, Writer, ReqData, LogData);
send_response_bodyfun({writer, WriterFun}, Code, all, ReqData, LogData) ->
    Writer = fun() -> send_writer_body(ReqData#wm_reqdata.socket, is_chunked_transfer(wrq:version(ReqData), chunked), WriterFun) end,
    send_response_headers(Code, undefined, chunked, Writer, ReqData, LogData);
send_response_bodyfun(Body, Code, all, ReqData, LogData) ->
    Length = iolist_size(Body),
    Writer = fun() -> send(ReqData#wm_reqdata.socket, Body), Length end,
    send_response_headers(Code, Length, undefined, Writer, ReqData, LogData);
send_response_bodyfun(Body, Code, Parts, ReqData, LogData) ->
    Writer = fun() -> send_parts(ReqData#wm_reqdata.socket, Body, Parts) end,
    send_response_headers(Code, undefined, undefined, Writer, ReqData, LogData).


send_response_headers(Code, Length, Transfer, Writer, ReqData, LogData) ->
    send(ReqData#wm_reqdata.socket,
        [make_version(wrq:version(ReqData)),
         make_code(Code), <<"\r\n">> | 
         make_headers(Code, Transfer, Length, ReqData)]),
    send_response_body_data(wrq:method(ReqData), Writer, ReqData, LogData).

send_response_body_data('HEAD', _Writer, ReqData, LogData) ->
    {ok, ReqData#wm_reqdata{log_data=LogData}};
send_response_body_data(_Method, Writer, ReqData, LogData) ->
    Written = Writer(),
    {ok, ReqData#wm_reqdata{log_data=LogData#wm_log_data{response_length=Written}}}.
    

send_stream_body(Socket, IsChunked, X) -> 
    send_stream_body(Socket, IsChunked, X, 0).

send_stream_body(Socket, IsChunked, {<<>>, done}, SoFar) ->
    send_chunk(Socket, IsChunked, <<>>),
    SoFar;
send_stream_body(Socket, IsChunked, {Data, done}, SoFar) ->
    Size = send_chunk(Socket, IsChunked, Data),
    send_chunk(Socket, IsChunked, <<>>),
    Size + SoFar;
send_stream_body(Socket, IsChunked, {{file, Filename}, Next}, SoFar) ->
    Length = filelib:file_size(Filename),
    send_stream_body(Socket, IsChunked, {{file, Length, Filename}, Next}, SoFar);
send_stream_body(Socket, IsChunked, {{file, 0, _Filename}, Next}, SoFar) ->
    send_stream_body(Socket, IsChunked, {<<>>, Next}, SoFar);
send_stream_body(Socket, IsChunked, {{file, Size, Filename}, Next}, SoFar) ->
    Bytes = send_file_body(Socket, IsChunked, Size, Filename),
    send_stream_body(Socket, IsChunked, {<<>>, Next}, Bytes + SoFar);
send_stream_body(Socket, IsChunked, {<<>>, Next}, SoFar) ->
    send_stream_body(Socket, IsChunked, Next(), SoFar);
send_stream_body(Socket, IsChunked, {[], Next}, SoFar) ->
    send_stream_body(Socket, IsChunked, Next(), SoFar);
send_stream_body(Socket, IsChunked, {Data, Next}, SoFar) ->
    Size = send_chunk(Socket, IsChunked, Data),
    send_stream_body(Socket, IsChunked, Next(), Size + SoFar).


send_writer_body(Socket, IsChunked, BodyFun) ->
    CounterPid = spawn_link(fun counter_process/0),
    Writer = fun(Data) ->
        Size = send_chunk(Socket, IsChunked, Data),
        CounterPid ! {sent, Size},
        Size
    end,
    BodyFun(Writer),
    send_chunk(Socket, IsChunked, <<>>),
    CounterPid ! {total, self()},
    receive
        {total, Total} ->
            Total
    end.

counter_process() ->
    counter_process_loop(0).

counter_process_loop(N) ->
    receive 
        {sent, N1} ->
            counter_process_loop(N+N1);
        {total, From} ->
            From ! {total, N}
    end.


send_device_body(Socket, Length, IO) ->
    send_file_body_loop(Socket, 0, Length, IO).

send_file_body(Socket, IsChunked, Length, Filename) ->
    send_chunk_start(Socket, IsChunked, Length),
    Bytes = send_file_body_1(mochiweb_socket:type(Socket), Socket, Length, Filename),
    send_chunk_end(Socket, IsChunked),
    Bytes.


send_file_body_1(ssl, Socket, Length, Filename) ->
    send_file_body_read(Socket, Length, Filename);
send_file_body_1(plain, Socket, Length, Filename) ->
    case use_sendfile() of
        disable ->
            send_file_body_read(Socket, Length, Filename);
        Type ->
            sendfile(Type, Socket, Filename, 0, Length)
    end.

send_file_body_read(Socket, Length, Filename) ->
    {ok, FD} = file:open(Filename, [raw,binary]),
    try
        send_file_body_loop(Socket, 0, Length, FD)
    after
        file:close(FD)
    end.

send_file_body_loop(_Socket, Offset, Size, _Device) when Offset =:= Size ->
    Size;
send_file_body_loop(Socket, Offset, Size, Device) when Size - Offset =< ?FILE_CHUNK_LENGTH ->
    {ok, Data} = file:read(Device, Size - Offset),
    send(Socket, Data),
    Size;
send_file_body_loop(Socket, Offset, Size, Device) ->
    {ok, Data} = file:read(Device, ?FILE_CHUNK_LENGTH),
    send(Socket, Data),
    send_file_body_loop(Socket, Offset+?FILE_CHUNK_LENGTH, Size, Device).


send_device_body_parts(Socket, {[{From,Length}], _Size, _Boundary, _ContentType}, IO) ->
    {ok, _} = file:position(IO, From), 
    send_file_body_loop(Socket, 0, Length, IO);
send_device_body_parts(Socket, {Parts, Size, Boundary, ContentType}, IO) ->
    Bytes = [
        begin
            {ok, _} = file:position(IO, From), 
            send(Socket, part_preamble(Boundary, ContentType, From, Length, Size)),
            B = send_file_body_loop(Socket, 0, Length, IO),
            send(Socket, <<"\r\n">>),
            B
        end
        || {From,Length} <- Parts
    ],
    send(Socket, end_boundary(Boundary)),
    lists:sum(Bytes).


send_file_body_parts(Socket, Parts, Filename) ->
    send_file_body_parts(mochiweb_socket:type(Socket), Socket, Parts, Filename).

send_file_body_parts(ssl, Socket, Parts, Filename) ->
    send_file_body_parts_read(Socket, Parts, Filename);
send_file_body_parts(plain, Socket, Parts, Filename) ->
    case use_sendfile() of
        disable ->
            send_file_body_parts_read(Socket, Parts, Filename);
        Type ->
            send_file_body_parts_sendfile(Type, Socket, Parts, Filename)
    end.

send_file_body_parts_sendfile(Type, Socket, {[{From,Length}], _Size, _Boundary, _ContentType}, Filename) ->
    sendfile(Type, Socket, Filename, From, Length);
send_file_body_parts_sendfile(Type, Socket, {Parts, Size, Boundary, ContentType}, Filename) ->
    Bytes = [
        begin
                send(Socket, part_preamble(Boundary, ContentType, From, Length, Size)),
                B = sendfile(Type, Filename, Socket, From, Length),
                send(Socket, <<"\r\n">>),
                B
        end
        || {From,Length} <- Parts
    ],
    send(Socket, end_boundary(Boundary)),
    lists:sum(Bytes).

send_file_body_parts_read(Socket, Parts, Filename) ->
    {ok, FD} = file:open(Filename, [raw,binary]),
    try
        send_device_body_parts(Socket, Parts, FD)
    after
        file:close(FD)
    end.

send_parts(Socket, Bin, {[{From,Length}], _Size, _Boundary, _ContentType}) ->
    send(Socket, binary:part(Bin,From,Length)),
    Length;
send_parts(Socket, Bin, {Parts, Size, Boundary, ContentType}) ->
    Bytes = [
        send_part_boundary(Socket, From, Length, Size, binary:part(Bin,From,Length), Boundary, ContentType)
        || {From,Length} <- Parts
    ],
    send(Socket, end_boundary(Boundary)),
    lists:sum(Bytes).

send_part_boundary(Socket, From, Length, Size, Bin, Boundary, ContentType) ->
    send(Socket, [
            part_preamble(Boundary, ContentType, From, Length, Size),
            Bin, <<"\r\n">>
        ]),
    size(Bin).


send_chunk(Socket, true, Data) ->
    Data1 = iolist_to_binary(Data),
    Size = size(Data1),
    send_chunk_start(Socket, true, Size),
    _ = send(Socket, Data1),
    send_chunk_end(Socket, true),
    Size;
send_chunk(_Socket, false, <<>>) ->
    0;
send_chunk(Socket, false, Data) ->
    Data1 = iolist_to_binary(Data),
    Size = size(Data1),
    _ = send(Socket, Data1),
    Size.

send_chunk_start(Socket, true, Size) when is_integer(Size) ->
    _ = send(Socket, mochihex:to_hex(Size)),
    _ = send(Socket, <<"\r\n">>);
send_chunk_start(_Socket, false, _Size) ->
    nop.

send_chunk_end(Socket, true) ->
    _ = send(Socket, <<"\r\n">>);
send_chunk_end(_Socket, false) ->
    nop.


send(undefined, _Data) ->
    ok;
send(Socket, Bin) when is_binary(Bin) ->
    case mochiweb_socket:send(Socket, Bin) of
        ok -> ok; 
        {error, closed} -> ok;
        _ -> exit(normal)
    end;
send(Socket, IoList) when is_list(IoList) ->
    send(Socket, iolist_to_binary(IoList)).


sendfile(erlang, Socket, Filename, Offset, Length) ->
    sendfile_erlang(Socket, Filename, Offset, Length).

sendfile_erlang(Socket, Filename, Offset, Length) ->
    {ok, FD} = file:open(Filename, [raw,binary]),
    try
        case file:sendfile(FD, Socket, Offset, Length, []) of
            {ok, Bytes} ->  Bytes;
            {error, closed} -> Length;
            _ -> exit(normal)
        end
    after
        file:close(FD)
    end.

get_resp_body_size({device, Size, _} = Body) ->
    {ok, Size, Body};
get_resp_body_size({device, IO}) ->
    Length = iodevice_size(IO),
    {ok, Length, {device, Length, IO}};
get_resp_body_size({file, Size, _} = Body) ->
    {ok, Size, Body};
get_resp_body_size({file, Filename}) ->
    Length = filelib:file_size(Filename),
    {ok, Length, {file, Length, Filename}};
get_resp_body_size(B) when is_binary(B) ->
    {ok, size(B), B};
get_resp_body_size(L) when is_list(L) ->
    B = iolist_to_binary(L),
    {ok, size(B), B};
get_resp_body_size(_) ->
    {error, nosize}.


%% @doc Infer incoming body length from transfer-encoding and content-length headers.
%% @todo Should support gzip/compressed transfer-encoding
body_length(ReqData) ->
    case get_header_value("transfer-encoding", ReqData) of
        undefined ->
            case get_header_value("content-length", ReqData) of
                undefined -> undefined;
                Length -> list_to_integer(Length)
            end;
        "chunked" -> chunked;
        Unknown -> {unknown_transfer_encoding, Unknown}
    end.

%% @doc Receive the body of the HTTP request (defined by Content-Length).
%%      Will only receive up to the default max-body length
do_recv_body(ReqData) ->
    MRB = ReqData#wm_reqdata.max_recv_body,
    read_whole_stream(recv_stream_body(ReqData, MRB), [], MRB, 0).

read_whole_stream({Hunk,_}, _, MaxRecvBody, SizeAcc)
  when SizeAcc + byte_size(Hunk) > MaxRecvBody -> 
    {error, req_body_too_large};
read_whole_stream({Hunk,Next}, Acc0, MaxRecvBody, SizeAcc) ->
    HunkSize = byte_size(Hunk),
    if SizeAcc + HunkSize > MaxRecvBody -> 
            {error, req_body_too_large};
       true ->
            Acc = [Hunk|Acc0],
            case Next of
                done -> iolist_to_binary(lists:reverse(Acc));
                _ -> read_whole_stream(Next(), Acc, MaxRecvBody, SizeAcc + HunkSize)
            end
    end.

recv_stream_body(ReqData, MaxHunkSize) ->
    put(mochiweb_request_recv, true),
    case get_header_value("expect", ReqData) of
        "100-continue" ->
            send(ReqData#wm_reqdata.socket, [make_version(wrq:version(ReqData)), make_code(100), <<"\r\n\r\n">>]);
        _Else ->
            ok
    end,
    case body_length(ReqData) of
        {unknown_transfer_encoding, X} -> exit({unknown_transfer_encoding, X});
        undefined -> {<<>>, done};
        0 -> {<<>>, done};
        chunked -> recv_chunked_body(ReqData#wm_reqdata.socket, MaxHunkSize);
        Length -> recv_unchunked_body(ReqData#wm_reqdata.socket, MaxHunkSize, Length)
    end.

recv_unchunked_body(Socket, MaxHunk, DataLeft) ->
    case MaxHunk >= DataLeft of
        true ->
            {ok,Data1} = mochiweb_socket:recv(Socket,DataLeft,?IDLE_TIMEOUT),
            {Data1, done};
        false ->
            {ok,Data2} = mochiweb_socket:recv(Socket,MaxHunk,?IDLE_TIMEOUT),
            {Data2,
             fun() -> recv_unchunked_body(Socket, MaxHunk, DataLeft-MaxHunk)
             end}
    end.
    
recv_chunked_body(Socket, MaxHunk) ->
    case read_chunk_length(Socket) of
        0 -> {<<>>, done};
        ChunkLength -> recv_chunked_body(Socket,MaxHunk,ChunkLength)
    end.
recv_chunked_body(Socket, MaxHunk, LeftInChunk) ->
    case MaxHunk >= LeftInChunk of
        true ->
            {ok,Data1} = mochiweb_socket:recv(Socket,LeftInChunk,?IDLE_TIMEOUT),
            {Data1,
             fun() -> recv_chunked_body(Socket, MaxHunk)
             end};
        false ->
            {ok,Data2} = mochiweb_socket:recv(Socket,MaxHunk,?IDLE_TIMEOUT),
            {Data2,
             fun() -> recv_chunked_body(Socket, MaxHunk, LeftInChunk-MaxHunk)
             end}
    end.

read_chunk_length(Socket) ->
    inet:setopts(Socket, [{packet, line}]),
    case mochiweb_socket:recv(Socket, 0, ?IDLE_TIMEOUT) of
        {ok, Header} ->
            inet:setopts(Socket, [{packet, raw}]),
            Splitter = fun (C) ->
                               C =/= $\r andalso C =/= $\n andalso C =/= $
                       end,
            {Hex, _Rest} = lists:splitwith(Splitter, binary_to_list(Header)),
            case Hex of
                [] -> 0;
                _ -> erlang:list_to_integer(Hex, 16)
            end;
        _ ->
            exit(normal)
    end.

get_range(#wm_reqdata{is_range_ok=false} = ReqData) ->
    {undefined, ReqData#wm_reqdata{range=undefined}};
get_range(ReqData) ->
    case get_header_value("range", ReqData) of
        undefined ->
            {undefined, ReqData#wm_reqdata{range=undefined}};
        RawRange ->
            Range = parse_range_request(RawRange),
            {Range, ReqData#wm_reqdata{range=Range}}
    end.


% Map request ranges to byte ranges.
range_parts(Ranges, Size) ->
    Ranges1 = [ range_skip_length(Spec, Size) || Spec <- Ranges ],
    [ R || R <- Ranges1, R =/= invalid_range ].

range_skip_length({none, R}, Size) when R =< Size, R >= 0 ->
    {Size - R, R};
range_skip_length({none, _OutOfRange}, Size) ->
    {0, Size};
range_skip_length({R, none}, Size) when R >= 0, R < Size ->
    {R, Size - R};
range_skip_length({_OutOfRange, none}, _Size) ->
    invalid_range;
range_skip_length({Start, End}, Size) when 0 =< Start, Start =< End, End < Size ->
    {Start, End - Start + 1};
range_skip_length({_OutOfRange, _End}, _Size) ->
    invalid_range.

parse_range_request(RawRange) when is_list(RawRange) ->
    try
        "bytes=" ++ RangeString = RawRange,
        Ranges = string:tokens(RangeString, ","),
        lists:map(fun ("-" ++ V)  ->
                          {none, list_to_integer(V)};
                      (R) ->
                          case string:tokens(R, "-") of
                              [S1, S2] ->
                                  {list_to_integer(S1), list_to_integer(S2)};
                              [S] ->
                                  {list_to_integer(S), none}
                          end
                  end,
                  Ranges)
    catch
        _:_ ->
            fail
    end.


get_range_headers([{Start, Length}], Size, _ContentType) ->
    HeaderList = [{"Accept-Ranges", "bytes"},
                  {"Content-Range", ["bytes ", make_io(Start), "-", make_io(Start+Length-1), "/", make_io(Size)]},
                  {"Content-Length", make_io(Length)}],
    {HeaderList, none};
get_range_headers(Parts, Size, ContentType) when is_list(Parts) ->
    Boundary = mochihex:to_hex(crypto:strong_rand_bytes(8)),
    Lengths = [
        iolist_size(part_preamble(Boundary, ContentType, Start, Length, Size)) + Length + 2
        || {Start,Length} <- Parts
    ],
    TotalLength = lists:sum(Lengths) + iolist_size(end_boundary(Boundary)), 
    HeaderList = [{"Accept-Ranges", "bytes"},
                  {"Content-Type", ["multipart/byteranges; ", "boundary=", Boundary]},
                  {"Content-Length", make_io(TotalLength)}],
    {HeaderList, Boundary}.

boundary(B)     -> [<<"--">>, B, <<"\r\n">>].
end_boundary(B) -> [<<"--">>, B, <<"--\r\n">>].

part_preamble(Boundary, CType, Start, Length, Size) ->
    [boundary(Boundary),
     <<"Content-Type: ">>, CType,
     <<"\r\nContent-Range: bytes ">>, make_io(Start), <<"-">>, make_io(Start+Length-1), <<"/">>, make_io(Size),
     <<"\r\n\r\n">>].


iodevice_size(IoDevice) ->
    {ok, Size} = file:position(IoDevice, eof),
    {ok, 0} = file:position(IoDevice, bof),
    Size.

make_io(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
make_io(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
make_io(Io) when is_list(Io); is_binary(Io) ->
    Io.

make_code(X) when is_integer(X) ->
    [integer_to_list(X), [" " | httpd_util:reason_phrase(X)]];
make_code(Io) when is_list(Io); is_binary(Io) ->
    Io.

make_version({1, 0}) ->
    <<"HTTP/1.0 ">>;
make_version(_) ->
    <<"HTTP/1.1 ">>.

make_headers(Code, Transfer, Length, RD) ->
    Hdrs0 = case Code of
        304 ->
            mochiweb_headers:make(wrq:resp_headers(RD));
        _ -> 
            LengthHeaders = case Length of
                undefined ->
                    mochiweb_headers:make(wrq:resp_headers(RD));
                _Length -> 
                    mochiweb_headers:enter(
                      "Content-Length",integer_to_list(Length),
                      mochiweb_headers:make(wrq:resp_headers(RD)))
            end,
            
            case is_chunked_transfer(wrq:version(RD), Transfer) of
                true ->
                    mochiweb_headers:enter(
                      "Transfer-Encoding","chunked",
                      LengthHeaders);
                false ->
                    LengthHeaders
            end
    end,
    {ok, ServerHeader} = application:get_env(webzmachine, server_header),
    WithSrv = mochiweb_headers:enter("Server", ServerHeader, Hdrs0),
    Hdrs = case mochiweb_headers:get_value("date", WithSrv) of
        undefined ->
            mochiweb_headers:enter("Date", httpd_util:rfc1123_date(), WithSrv);
        _ ->
            WithSrv
    end,
    F = fun({K, V}, Acc) ->
                [make_io(K), <<": ">>, V, <<"\r\n">> | Acc]
        end,
    lists:foldl(F, [<<"\r\n">>], mochiweb_headers:to_list(Hdrs)).


is_chunked_transfer(HttpVersion, chunked) when HttpVersion > {1,0} -> true;
is_chunked_transfer(_, _) -> false.


server_header() ->
    "MochiWeb/1.1 WebZMachine/" ++ ?WMVSN.

socket(ReqData) -> ReqData#wm_reqdata.socket.

method(ReqData) -> wrq:method(ReqData).

version(ReqData) -> wrq:version(ReqData).

disp_path(ReqData) -> wrq:disp_path(ReqData).

path(ReqData) -> wrq:path(ReqData).

raw_path(ReqData) -> wrq:raw_path(ReqData).

req_headers(ReqData) -> wrq:req_headers(ReqData).
headers(ReqData) -> req_headers(ReqData).

req_body(MaxRecvBody, ReqData) ->
    case ReqData#wm_reqdata.bodyfetch of
        stream ->
            {stream_conflict, ReqData};
        _ ->
            NewReqData = ReqData#wm_reqdata{max_recv_body=MaxRecvBody},
            case NewReqData#wm_reqdata.req_body of
                not_fetched_yet ->
                    NewBody = do_recv_body(NewReqData),
                    NewRD = NewReqData#wm_reqdata{req_body=NewBody,bodyfetch=standard},
                    {NewBody, NewRD};
                X ->
                    {X, ReqData#wm_reqdata{bodyfetch=standard}}
            end
    end.

stream_req_body(MaxHunk, ReqData) -> 
    case ReqData#wm_reqdata.bodyfetch of
        standard ->
            {stream_conflict, ReqData};
        _ ->
            {recv_stream_body(ReqData, MaxHunk), ReqData#wm_reqdata{bodyfetch=stream}}
    end.

resp_headers(ReqData) -> wrq:resp_headers(ReqData).
out_headers(ReqData) -> resp_headers(ReqData).

get_resp_header(HdrName, ReqData) -> mochiweb_headers:get_value(HdrName, wrq:resp_headers(ReqData)).
get_out_header(HeaderName, ReqData) -> get_resp_header(HeaderName, ReqData).

has_resp_header(HeaderName, ReqData) ->
    case get_out_header(HeaderName, ReqData) of
        undefined -> false;
        _ -> true
    end.
has_out_header(HeaderName, ReqData) -> has_resp_header(HeaderName, ReqData).

has_resp_body(ReqData) ->
    case wrq:resp_body(ReqData) of
        undefined -> false;
        <<>> -> false;
        [] -> false;
        _ -> true
    end.
has_response_body(ReqData) -> 
    has_resp_body(ReqData).

response_code(ReqData) -> wrq:response_code(ReqData).
set_response_code(Code, ReqData) -> wrq:set_response_code(Code, ReqData).

peer(ReqData) -> get_peer(ReqData).

range(ReqData) -> get_range(ReqData).

req_cookie(ReqData) -> wrq:req_cookie(ReqData).
parse_cookie(ReqData) -> req_cookie(ReqData).
get_cookie_value(Key, ReqData) -> proplists:get_value(Key, req_cookie(ReqData)).

req_qs(ReqData) -> wrq:req_qs(ReqData).
parse_qs(ReqData) -> req_qs(ReqData).
get_qs_value(Key, ReqData) -> proplists:get_value(Key, req_qs(ReqData)).
get_qs_value(Key, Default, ReqData) -> proplists:get_value(Key, req_qs(ReqData), Default).

set_resp_body(Body, ReqData) -> wrq:set_resp_body(Body, ReqData).

resp_body(ReqData) -> wrq:resp_body(ReqData).
response_body(ReqData) -> resp_body(ReqData).

get_req_header(K, ReqData) -> wrq:get_req_header(K, ReqData).

set_resp_header(K, V, ReqData) -> wrq:set_resp_header(K, V, ReqData).
add_response_header(K, V, ReqData) -> set_resp_header(K, V, ReqData).

set_resp_headers(Hdrs, ReqData) ->  wrq:set_resp_headers(Hdrs, ReqData).
add_response_headers(Hdrs, ReqData) -> set_resp_headers(Hdrs, ReqData).

remove_resp_header(K, ReqData) -> wrq:remove_resp_header(K, ReqData).
remove_response_header(K, ReqData) -> remove_resp_header(K, ReqData).

merge_resp_headers(Hdrs, ReqData) -> wrq:merge_resp_headers(Hdrs, ReqData).
merge_response_headers(Hdrs, ReqData) -> merge_resp_headers(Hdrs, ReqData).

append_to_response_body(Data, ReqData) -> wrq:append_to_response_body(Data, ReqData).

do_redirect(ReqData) -> wrq:do_redirect(true, ReqData).

resp_redirect(ReqData) -> wrq:resp_redirect(ReqData).

get_metadata('chosen-charset', ReqData) ->
     wrq:resp_chosen_charset(ReqData);
get_metadata('content-encoding', ReqData) ->
     wrq:resp_content_encoding(ReqData);
get_metadata('transfer-encoding', ReqData) ->
     wrq:resp_transfer_encoding(ReqData);
get_metadata('content-type', ReqData) ->
     wrq:resp_content_type(ReqData);
get_metadata(Key, ReqData) ->
    case dict:find(Key, ReqData#wm_reqdata.metadata) of
                {ok, Value} -> Value;
                error -> undefined
        end.

set_metadata(Key, Value, ReqData) ->
    NewDict = dict:store(Key, Value, ReqData#wm_reqdata.metadata),
    {ok, ReqData#wm_reqdata{metadata=NewDict}}.

get_path_info(ReqData) -> dict:to_list(wrq:path_info(ReqData)).

get_path_info(Key, ReqData) -> wrq:path_info(Key, ReqData).

path_tokens(ReqData) -> wrq:path_tokens(ReqData).
get_path_tokens(ReqData) -> path_tokens(ReqData).

app_root(ReqData) -> wrq:app_root(ReqData).
get_app_root(ReqData) -> app_root(ReqData).

load_dispatch_data(Bindings, HostTokens, Port, PathTokens, AppRoot, DispPath, ReqData) ->
    PathInfo = dict:from_list(Bindings),
    RD1 = wrq:load_dispatch_data(
                        PathInfo,HostTokens,Port,PathTokens,AppRoot,
                        DispPath,ReqData),
    {ok, RD1}.


log_data(#wm_reqdata{log_data=LogData, metadata=MetaData}) ->
    LogData#wm_log_data{metadata=MetaData}.

use_sendfile() ->
    case application:get_env(webzmachine, use_sendfile) of
        undefined -> disable;
        {ok, disable} -> disable;
        {ok, erlang} -> erlang
    end.
