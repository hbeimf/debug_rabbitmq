%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 4.0.2

-ifndef(msg_proto).
-define(msg_proto, true).

-define(msg_proto_gpb_version, "4.0.2").

-ifndef('MSG_PB_H').
-define('MSG_PB_H', true).
-record('Msg',
        {action = 0             :: non_neg_integer() | undefined, % = 1, 32 bits
         msgBody = <<>>         :: binary() | undefined % = 2
        }).
-endif.

-endif.
