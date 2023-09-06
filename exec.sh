#! /bin/bash

swipl -l $1 -t 'set_prolog_flag(answer_write_options,[max_depth(0)]),leash(-all),trace,test' &> tmp.txt &
sleep 3
killall swipl

grep -vwE "(member\(.*\)|member_set|subset|remove_from_set|member_state|equal_set)" tmp.txt > file.txt 

