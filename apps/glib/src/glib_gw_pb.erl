-module(glib_gw_pb).
-compile(export_all).

% -include_lib("glib/include/action.hrl").
-include_lib("glib/include/gw_proto.hrl").
-include_lib("glib/include/log.hrl").

% message VerifyReq { //请求认证 cmd=3  http
%                     string identity = 1; //用户身份
%                     string channel_id = 2;
% }
encode_VerifyReq(Identity, Channel_id)->
	VerifyReq = #'VerifyReq'{
        identity = Identity
		,channel_id = Channel_id
	},
	Pb = gw_proto:encode_msg(VerifyReq),
	Pb.

decode_VerifyReq(DataBin) -> 
	#'VerifyReq'{identity = Identity, channel_id = Channel_id} = gw_proto:decode_msg(DataBin, 'VerifyReq'),
	{Identity, Channel_id}.

% message VerifyRes { //请求认证回复 cmd=4 供客户端使用
%                     int32 code = 1; //错误编码， 0：成功， 非0：失败
%                     string uid = 2; //如果id 不为 ""， 说明
%                     string msg = 3;
% }
encode_VerifyRes(Code, Uid, Msg)->
	VerifyRes = #'VerifyRes'{
        code = Code
        , uid = Uid
        , msg = Msg
	},
	Pb = gw_proto:encode_msg(VerifyRes),
	Pb.

