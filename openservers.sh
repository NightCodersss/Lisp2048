#!/bin/bash

(echo 1080 | mit-scheme --load server.com) &
(echo 1081 | mit-scheme --load server.com) &
(echo 1082 | mit-scheme --load server.com) &
(echo 1083 | mit-scheme --load server.com) &
