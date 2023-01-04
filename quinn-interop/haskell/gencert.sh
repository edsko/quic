#!/bin/bash

openssl req -x509 -sha256 -days 365 -nodes -out server.cert -keyout server.priv -subj "/CN=simple_server"
