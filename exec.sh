#! /bin/bash

swipl -l $1 -t 'leash(-all),trace,test' &> tmp.txt &
sleep 3
killall swipl

grep -vwE "(member\(.*\)|member_set|subset|remove_from_set)" tmp.txt > file.txt 

