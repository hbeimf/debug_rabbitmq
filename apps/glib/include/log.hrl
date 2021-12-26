%% connect 
% -define(LOG(X), io:format("~n==========log begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
-define(LOG(X), true).

%% data receive
-define(LOG1(X), io:format("==========log begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
% -define(LOG1(X), true).
