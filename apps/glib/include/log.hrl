%% rabbit 先从　rabbit:start(normal, []). 启动
%%　第一:　实始化一些　actor,  ets, mnesia 表　
%%　第二:　启动　ranch　相关的网络包,至此将启动状态设置为　ready, 说明已经准备好接收来自客户端的连接请求了　.

%% connect 
% -define(LOG(X), io:format("~n==========log begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
-define(LOG(X), true).

%% data receive && login
-define(LOG1(X), io:format("==========log begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
% -define(LOG1(X), true).


% -define(LOG2(X), io:format("==========log begin========{~p,~p}==============~n~p~n~n", [?MODULE,?LINE,X])).
-define(LOG2(X), true).