#!/bin/sh -x

openssl aes-256-cbc -K $encrypted_05f822b6caa8_key -iv $encrypted_05f822b6caa8_iv -in travis-deploy-key.enc -out travis-deploy-key -d
chmod 600 travis-deploy-key
cp travis-deploy-key ~/.ssh/id_rsa
