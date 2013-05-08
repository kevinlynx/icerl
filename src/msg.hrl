
-ifndef(ICEMSG_HRL).
-define(ICEMSG_HRL, true).

-record(header, {
        magic = "IceP",
        protoMajor = 1, protoMinor = 0,
        encodeMajor = 1, encodeMinor = 0,
        msgtype, comStatus = 0,
            msgsize = 14}).

-record(encap, {
        size = 0,
        major = 1, minor = 1,
        payload = <<>>}).

-record(identity, {name, cate = ""}).
         
-record(request, {
        req,
        id,
        facet = [],
        operation,
        mode = 0,
        context = [],
        encap}).

-record(reply, {
        req, status = 0, encap}).

-define(MSG_REQUEST, 0).
-define(MSG_REPLY, 2).
-define(MSG_VALIDATE_CONN, 3).
-define(MSG_CLOSE, 4).

-define(REPLY_SUCCESS, 0).
-define(REPLY_OBJ_NOT_FOUND, 2).
-define(REPLY_METHOD_NOT_FOUND, 4).

-endif.

