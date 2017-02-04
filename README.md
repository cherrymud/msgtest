# README for msgtest

This is to help debug the intermittent error with [cl-nats](https://github.com/tormaroe/cl-nats).

## Final Update

It looks like the bug was caused by attempting to subscribe and publish before the NATS connection was fully established.

The fix seems to be:

````
(defvar conn (nats:make-connection))
(nats:wait-for-connection conn)
<do stuff here>
````

## Environment

* Ubuntu 16.04 x64
* SBCL 1.3.14
* flexi-streams 1.0.15
* usocket 0.7.0.1
* cl-nats master
* Go 1.7.4
* [gnatsd](https://github.com/nats-io/gnatsd) 0.9.6 (built from source)

## Replicating the issue

* Clone this repository
* "make"
* Start gnatsd with -DV option for tracing output
* Run ./msgtest until it breaks

*Example:*

````
$ ./msgtest
Starting..
>> SUB events 1
<< INFO {"server_id":"7PEcWJmJmnYkulEpaMYr7x","version":"0.9.6","go":"go1.7.4","host":"0.0.0.0","port":4222,"auth_required":false,"ssl_required":false,"tls_required":false,"tls_verify":false,"max_payload":1048576}
>> CONNECT {"name":"","lang":"LISP","version":"0.1.1","verbose":true}
<< -ERR 'Parser Error'
end of file on #<FLEXI-STREAMS:FLEXI-IO-STREAM {1002C9B163}>
;
; compilation unit aborted
;   caught 1 fatal ERROR condition
````

*Output from gnatsd at the same time:*

````
$ gnatsd -DV
[9336] 2017/02/04 12:43:30.197644 [INF] Starting nats-server version 0.9.6
[9336] 2017/02/04 12:43:30.197718 [DBG] Go build version go1.7.4
[9336] 2017/02/04 12:43:30.197726 [INF] Listening for client connections on 0.0.0.0:4222
[9336] 2017/02/04 12:43:30.197796 [DBG] Server id is 7PEcWJmJmnYkulEpaMYr7x
[9336] 2017/02/04 12:43:30.197806 [INF] Server is ready
[9336] 2017/02/04 12:43:41.865663 [DBG] 127.0.0.1:51260 - cid:1 - Client connection created
[9336] 2017/02/04 12:43:41.980599 [TRC] 127.0.0.1:51260 - cid:1 - ->> [CONNECT {"name":"","lang":"LISP","version":"0.1.1","verbose":true}SUB events 1]
[9336] 2017/02/04 12:43:41.980645 [ERR] 127.0.0.1:51260 - cid:1 - Error reading from client: invalid character 'S' after top-level value
[9336] 2017/02/04 12:43:41.980662 [TRC] 127.0.0.1:51260 - cid:1 - <<- [-ERR Parser Error]
[9336] 2017/02/04 12:43:41.980716 [DBG] 127.0.0.1:51260 - cid:1 - Client connection closed
````

## Initial observations

It appears as though the gnatsd server is receiving two NATS protocol commands at the same time:

CONNECT {"name":"","lang":"LISP","version":"0.1.1","verbose":true}**SUB events 1**

Looking at [nats.io.lisp](https://github.com/tormaroe/cl-nats/blob/master/nats.io.lisp), it seems like newlines should be getting sent:

````
(defun nats-write (stream msg)
  (when nats.vars::*debug*
    (format *trace-output* "~&>> ~A~%" msg))
  (format stream "~A~%" msg)
(force-output stream))
````

And looking at [nats.lisp](https://github.com/tormaroe/cl-nats/blob/master/nats.lisp), it appears that flexi-streams newlines should be getting sent as crlf:

````
(defun connect (connection)
  ""
  ; TODO: disconnect first if needed
  (let* ((socket (usocket:socket-connect (host-of connection)
                                         (port-of connection)
                                         :element-type '(unsigned-byte 8)))
         (stream (flexi-streams:make-flexi-stream (usocket:socket-stream socket)
                                                  :external-format
                                                  (flexi-streams:make-external-format
                                                    *encoding* :eol-style :crlf))))
    (setf (socket-of connection) socket)
    (setf (stream-of connection) stream)
    (setf (thread-of connection) (make-reader-thread connection))
connection))
````

Reading the [NATS Protocol](http://nats.io/documentation/internals/nats-protocol/) documentation:

````
Newlines: Like other text-based protocols, NATS uses CR followed by LF (CR+LF, \r\n, 0x0D0A) to terminate protocol messages. This newline sequence is also used to mark the beginning of the actual message payload in a PUB or MSG protocol message.
````

## Conclusion

Not sure what's going on, but that's enough information to start debugging further.
