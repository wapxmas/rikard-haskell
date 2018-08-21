#!/bin/bash
stack build --stack-yaml stack-linux.yaml
echo "Restarting ... "
stop rikard
sleep 3
start rikard
