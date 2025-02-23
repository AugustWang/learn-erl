%% Author: liuwei
%% Created: 2011-2-20
%% Description: TODO: Add description to mod_monster_effect
-module(mod_pet_effect).

%%
%% API Functions
%%
-export([
         handle/1,
         reduce_hp/4,
         add_hp/1
        ]).

-include("mgeem.hrl").


handle({EffectFunc, EffectValue, SrcID, SrcType, SrcName}) -> 
    case catch apply(?MODULE, EffectFunc, [EffectValue, SrcID, SrcType, SrcName]) of 
    {'EXIT', Reason} -> 
        ?ERROR_MSG("call ~w:~w (~w, ~w, ~w, ~w) failed -> ~w", 
                   [?MODULE, EffectFunc, EffectValue, SrcID, SrcType, SrcName, Reason]),
        ok;
    _ ->
        ok
    end.


reduce_hp(EffectValue, PetInfo, SrcActorID, SrcActorType) ->
    mod_map_pet:pet_reduce_hp(EffectValue, PetInfo#p_pet.pet_id, SrcActorID, SrcActorType).


add_hp(_Increment) ->
    ok.


%%
%% Local Functions
%%
