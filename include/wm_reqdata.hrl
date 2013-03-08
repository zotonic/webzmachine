-record(wm_reqdata, {
    % Reqstate
    socket=undefined,
    metadata=dict:new(),
    range=undefined,
    peer=undefined :: inet:ip_address(),
    bodyfetch=undefined,
    log_data=undefined,

    % Reqdata
    method  :: wrq:method(),
    scheme  :: wrq:scheme(),
    version :: wrq:version(),
    disp_path,
    path     :: string(),
    raw_path :: string(),
    path_info :: dict(),
    path_tokens :: [string()],
    app_root  :: string(),
    response_code :: pos_integer(),
    max_recv_body :: pos_integer(),
    req_cookie    :: string(),
    req_qs        :: string(),
    req_headers   :: gb_tree(), %% mochiheaders
    req_body,
    resp_redirect :: boolean(),
    resp_headers,
    resp_body  :: any(),
    resp_range :: atom(),
    host_tokens  :: [string()],
    port  :: inet:port_number(),

	%% Cache of resource calls
	cache=[] :: list()
}).

-record(wm_controller, {
    mod,
    mod_state,
    mod_exports, 
    trace
}).

-define(WM_DBG(Msg), error_logger:info_msg("DEBUG: ~p:~p  ~p~n", [?MODULE, ?LINE, Msg])).
