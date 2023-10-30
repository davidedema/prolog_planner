#! /bin/bash

swipl -l $1 -t 'testNoTrace' &> tmp.txt &
sleep 3
killall swipl
ret=$?
if [ $ret == 0 ]; then 
  echo "Executing for 10s"
  swipl -l $1 -t 'set_prolog_flag(debugger_write_options,[max_depth(0)]),testTrace' &> tmp.txt &
  sleep 5                                                                   
  killall swipl                                                              
else                                                                         
  echo "Executing until end"                                                 
  swipl -l $1 -t 'set_prolog_flag(debugger_write_options,[max_depth(0)]),testTrace' &> tmp.txt
fi

grep -vwE "(member\(.*\)|member_set|subset|remove_from_set|member_state|equal_set)" tmp.txt > file.txt 

