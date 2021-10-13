#!/bin/sh
createuser pkr
createdb -O pkr pkr
psql -U pkr -d pkr -f postgresql.sql
