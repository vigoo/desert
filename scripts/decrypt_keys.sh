#!/bin/sh -x

openssl aes-256-cbc -K $encrypted_aa8e06d836e3_key -iv $encrypted_aa8e06d836e3_iv -in travis-deploy-key.enc -out travis-deploy-key -d
chmod 600 travis-deploy-key
cp travis-deploy-key ~/.ssh/id_rsa
