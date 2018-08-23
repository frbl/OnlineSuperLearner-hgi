#!/usr/bin/env sh
#rsync -ue ssh ./ peregrine:~/osl

server_name=peregrine-interactive
scp -r * $server_name:~/hgi
ssh $server_name -t 'rm -rf ~/tmp/hgi; mkdir -p ~/tmp/hgi; rm -rf OnlineSuperLearner; cp -r osl OnlineSuperLearner; cd hgi; Rscript main.R'
rm -r /tmp/hgi/
mkdir -p /tmp/hgi/
scp -r $server_name:~/tmp/* /tmp/hgi/
