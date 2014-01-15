%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2009 Basho Technologies
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

-module(webmachine_controller).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-author('Marc Worrell <marc@worrell.nl>').

-export([
    do/3,
    log_d/2,
    stop/2,

    init/3,
    modstate/1
]).

-include("../include/wm_reqdata.hrl").
-include("../include/webmachine_logger.hrl").

default(ping) ->
    no_default;
default(service_available) ->
    true;
default(resource_exists) ->
    true;
default(auth_required) ->
    true;
default(is_authorized) ->
    true;
default(forbidden) ->
    false;
default(upgrades_provided) ->
    [];
default(allow_missing_post) ->
    false;
default(malformed_request) ->
    false;
default(uri_too_long) ->
    false;
default(known_content_type) ->
    true;
default(valid_content_headers) ->
    true;
default(valid_entity_length) ->
    true;
default(options) ->
    [];
default(allowed_methods) ->
    ['GET', 'HEAD'];
default(known_methods) ->
    ['GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'TRACE', 'CONNECT', 'OPTIONS'];
default(content_types_provided) ->
    [{"text/html", to_html}];
default(content_types_accepted) ->
    [];
default(delete_resource) ->
    false;
default(delete_completed) ->
    true;
default(post_is_create) ->
    false;
default(create_path) ->
    undefined;
default(base_uri) ->
    undefined;
default(process_post) ->
    false;
default(language_available) ->
    true;

% The default setting is needed for non-charset responses such as image/png
% An example of how one might do actual negotiation:
%    ["iso-8859-1", "utf-8"];
default(charsets_provided) ->
    no_charset; % this atom causes charset-negotation to short-circuit

% The content variations available to the controller.
default(content_encodings_provided) ->
    ["identity"];

% How the content is transferred, this is handy for auto-gzip of GET-only resources.
% "identity" and "chunked" are always available to HTTP/1.1 clients.
% Example:
%    [{"gzip", fun(X) -> zlib:gzip(X) end}];
default(transfer_encodings_provided) ->
    [{"gzip", fun(X) -> zlib:gzip(X) end}];

default(variances) ->
    [];
default(is_conflict) ->
    false;
default(multiple_choices) ->
    false;
default(previously_existed) ->
    false;
default(moved_permanently) ->
    false;
default(moved_temporarily) ->
    false;
default(last_modified) ->
    undefined;
default(expires) ->
    undefined;
default(generate_etag) ->
    undefined;
default(finish_request) ->
    true;
default(_) ->
    no_default.
          
init(ReqData, Mod, Args) ->
    {ok, ModState} = Mod:init(Args),
    [{trace_dir, Dir}] = ets:lookup(?WMTRACE_CONF_TBL, trace_dir),
    ToTrace = case {ets:lookup(?WMTRACE_CONF_TBL, trace_global), 
                    ets:lookup(?WMTRACE_CONF_TBL, Mod)} of
                  {_, [{Mod, Eagerness_}]} ->
                      {true, Eagerness_};
                  {[{trace_global, Policy}], _} ->
                      PolicyFun = 
                          case Policy of
                              '5xx' ->
                                  fun(RspCode) ->
                                          if 
                                              RspCode >= 500  
                                                  andalso RspCode =< 599 -> 
                                                  flush;
                                              true -> drop
                                          end
                                  end;
                              '4xx&5xx' -> 
                                  fun(RspCode) ->
                                          if 
                                              RspCode >= 400  
                                                  andalso RspCode =< 599 -> 
                                                  flush;
                                              true -> drop
                                          end
                                  end                                 
                          end,
                      {true, {at_once, PolicyFun}};
                  {_, _} ->
                      false
              end,
    case ToTrace of
        false ->
            {ok, #wm_controller{
                    mod=Mod,
                    mod_state=ModState,
                    mod_exports=[ F || {F,_} <- Mod:module_info(exports) ],
                    trace=false}};
        {true, Eagerness} ->
            {ok, LoggerProc} = start_log_proc(Dir, Mod, Eagerness),
            ReqId = (ReqData#wm_reqdata.log_data)#wm_log_data.req_id,        
            log_reqid(LoggerProc, ReqId),
            log_decision(LoggerProc, v3b14),
            log_call(LoggerProc, attempt, Mod, init, Args),
            log_call(LoggerProc, result, Mod, init, {{trace, Dir}, ModState}),
            {ok, #wm_controller{
                    mod=Mod,
                    mod_state=ModState,
                    mod_exports=[ F || {F,_} <- Mod:module_info(exports) ],
                    trace=LoggerProc}};
        _ ->
            {stop, bad_init_arg}
    end.

modstate(Controller) ->
    Controller#wm_controller.mod_state.

do(Fun, #wm_controller{mod_exports=Exports} = Controller, ReqData) when is_atom(Fun) ->
    case lists:member(Fun, Exports) of
        true ->
            controller_call(Fun, Controller, ReqData);
        false ->
            case default(Fun) of
                no_default -> 
                    controller_call(Fun, Controller, ReqData);
                Default ->
                    log_call(Controller#wm_controller.trace,
                             no_exported,
                             Controller#wm_controller.mod,
                             Fun,
                             [ReqData, Controller#wm_controller.mod_state]),
                    {Default, Controller, ReqData}
            end
    end.

controller_call(F, Controller, ReqData) ->
    log_call(Controller#wm_controller.trace,
             attempt, 
             Controller#wm_controller.mod,
             F,
             [ReqData, Controller#wm_controller.mod_state]),
    Result = try
                {Res,RD1,ModState1} = apply(Controller#wm_controller.mod, F, [ReqData, Controller#wm_controller.mod_state]),
                {Res, Controller#wm_controller{mod_state=ModState1}, RD1}
             catch C:R ->
                Reason = {C, R, erlang:get_stacktrace()},
                {{error, Reason}, Controller, ReqData}
             end,
    log_call(Controller#wm_controller.trace,
             attempt, 
             Controller#wm_controller.mod,
             F,
             Result),
    Result.

log_d(#wm_controller{trace=Trace}, DecisionID) ->
    log_decision(Trace, DecisionID).

stop(#wm_controller{trace=Trace}, ReqData) ->
    stop_log_proc(Trace, ReqData).


log_reqid(false, _ReqId) ->
    nop;
log_reqid(LoggerProc, ReqId) ->
    z_logger:log(LoggerProc, 5, "{req_id, ~p}.~n", [ReqId]).

log_decision(false, _DecisionID) ->
    nop;
log_decision(LoggerProc, DecisionID) ->
    z_logger:log(LoggerProc, 5, "{decision, ~p}.~n", [DecisionID]).

log_call(false, _Type, _M, _F, _Data) ->
    nop;
log_call(LoggerProc, Type, M, F, Data) ->
    z_logger:log(LoggerProc, 5,
                 "{~p, ~p, ~p,~n ~p}.~n",
                 [Type, M, F, escape_trace_data(Data)]).

escape_trace_data(Fun) when is_function(Fun) ->
    {'WMTRACE_ESCAPED_FUN',
     [erlang:fun_info(Fun, module),
      erlang:fun_info(Fun, name),
      erlang:fun_info(Fun, arity),
      erlang:fun_info(Fun, type)]};
escape_trace_data(Pid) when is_pid(Pid) ->
    {'WMTRACE_ESCAPED_PID', pid_to_list(Pid)};
escape_trace_data(Port) when is_port(Port) ->
    {'WMTRACE_ESCAPED_PORT', erlang:port_to_list(Port)};
escape_trace_data(List) when is_list(List) ->
    escape_trace_list(List, []);
escape_trace_data(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(escape_trace_data(tuple_to_list(Tuple)));
escape_trace_data(Other) ->
    Other.

escape_trace_list([Head|Tail], Acc) ->
    escape_trace_list(Tail, [escape_trace_data(Head)|Acc]);
escape_trace_list([], Acc) ->
    %% proper, nil-terminated list
    lists:reverse(Acc);
escape_trace_list(Final, Acc) ->
    %% non-nil-terminated list, like the dict module uses
    lists:reverse(tl(Acc))++[hd(Acc)|escape_trace_data(Final)].

start_log_proc(Dir, Mod, Eagerness) ->
    Now = {_,_,US} = os:timestamp(),
    {{Y,M,D},{H,I,S}} = calendar:now_to_universal_time(Now),
    Filename = io_lib:format(
        "~s/~p-~4..0B-~2..0B-~2..0B"
        "-~2..0B-~2..0B-~2..0B.~6..0B.wmtrace",
        [Dir, Mod, Y, M, D, H, I, S, US]),
    z_logger:start([{output, {file, Filename}},
                    {eagerness, Eagerness}, {loglevel, 5}]).

stop_log_proc(LogProc, ReqData) when is_pid(LogProc) and is_tuple(ReqData) ->
    ResponseCode = (ReqData#wm_reqdata.log_data)#wm_log_data.response_code,
    z_logger:stop(LogProc, ResponseCode);
stop_log_proc(_, _) ->
    ok.
