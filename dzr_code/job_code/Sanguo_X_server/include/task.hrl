-ifndef(__TASK_HRL__).
-define(__TASK_HRL__, 1).

-include("spec_types.hrl").

-define(TASK_TYPE_MAIN,     1).
-define(TASK_TYPE_BRANCH,   2).
-define(TASK_TYPE_CYCLIC_MIN,   11).
-define(TASK_TYPE_CYCLIC_MAX,   19).

-define(TASK_TYPE_CYCLIC_SCHOOL, 11).
-define(TASK_TYPE_CYCLIC_GANK,   12).

-define(TASK_REWARD_EXP,    3).
-define(TASK_REWARD_ITEM,   4).
-define(TASK_REWARD_RECRUIT,    5).
-define(TASK_REWARD_MAP,    6).
-define(TASK_REWARD_MERIT,  7).
-define(TASK_REWARD_SCHOOL_POINT,   8).     %% 'SCHOOL' = 师门
-define(TASK_REWARD_GANG_POINT, 9).         %% 'GANG' = 帮派
-define(TASK_REWARD_DAILY_POINT,    10).    %% 日常积分
-define(TASK_REWARD_MONEY,  11).            %% 金钱

-record(task_reward,
    {
        type    ::  
                    ?TASK_REWARD_EXP | ?TASK_REWARD_ITEM | 
                    ?TASK_REWARD_RECRUIT | ?TASK_REWARD_MAP |
                    ?TASK_REWARD_MERIT | ?TASK_REWARD_SCHOOL_POINT |
                    ?TASK_REWARD_GANG_POINT | ?TASK_REWARD_DAILY_POINT,
        value   :: any()    %% depends on the 'type' field
    }).

-record(task_tip,
    {
        key     :: {task_req_type(), integer()},        %% {ReqType, ID}
        need    :: integer(),
        finish  :: integer()
    }).

-record(task, 
    {
        id          :: task_id(),
        name        :: string(),
        type        :: ?TASK_TYPE_MAIN | ?TASK_TYPE_BRANCH | 
                       ?TASK_TYPE_CYCLIC_MIN..?TASK_TYPE_CYCLIC_MAX,
        req_type    :: task_req_type(),
        difficulty  :: integer(),
        req_level   :: integer(),
        tips        :: [#task_tip{}],
        prev_id     :: task_id(),
        npc1        :: npc_id(),
        npc2        :: npc_id(),
        rec_reward  :: [#task_reward{}],
        reward      :: [#task_reward{}],
        auto_complete
    }).

-record(task_state,
    {
        id      :: task_id(),
        tips    :: [#task_tip{}]
    }).

-record(player_task,
    {
        gd_accountID        :: player_id(),
        gd_completedList    :: [integer()],
        gd_receivedList     :: [#task_state{}],
        gd_curCyclicList    :: [integer()],
        gd_lastTime = 0
    }).

-record(player_task_types,
    {
        gd_accountID        = {integer},
        gd_completedList    = {term},
        gd_receivedList     = {term},
        gd_curCyclicList    = {term},
        gd_lastTime         = {integer}
    }).

-endif.

