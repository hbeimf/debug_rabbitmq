-define(LOG(X, Desc), io:format("~p========================================~n~ts~nＭod: ~p; Line: ~p ;~n~p~n~n", [glib:date_str(), Desc, ?MODULE,?LINE,X])).
%%-define(LOG(X, D), true).







