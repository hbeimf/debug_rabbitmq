%% connect 
% -define(LOG(X), io:format("~n==========log begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
-define(LOG(X), true).

%% data receive && login
% -define(LOG1(X), io:format("==========log begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
-define(LOG1(X), true).


-define(LOG2(X), io:format("==========log begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
% -define(LOG2(X), true).