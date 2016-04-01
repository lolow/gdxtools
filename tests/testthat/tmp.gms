$offdigit
$onempty
set r /
iron
nickel
/;
set p /
nuts
bolts
washers
/;
set t /
1
2
3
4
/;
set tl /
1
2
3
4
5
/;
parameter b(r)  'initial stock' /
iron 35.80
nickel  7.32
/;
parameter c(p,t)  'profit' /
nuts.1 1.73
nuts.2 1.80
nuts.3 1.60
nuts.4 2.20
bolts.1 1.82
bolts.2 1.90
bolts.3 1.70
bolts.4 0.95
washers.1 1.05
washers.2 1.10
washers.3 0.95
washers.4 1.33
/;
parameter d(r)  'storage cost' /
iron 0.030
nickel 0.025
/;
variable x(p,tl)  '' ;
variable s(r,tl)  '' ;
variable profit '' ;
x.l('bolts','2')=43.0044444444444522;
x.l('washers','4')= 0.1155555555555533;
s.l('iron','1')=35.799999999999997158;
s.l('iron','2')=35.799999999999997158;
s.l('iron','3')= 0.106311111111109080;
s.l('iron','4')= 0.106311111111109080;
s.l('nickel','1')= 7.320000000000000284;
s.l('nickel','2')= 7.320000000000000284;
s.l('nickel','3')= 0.009244444444442834;
s.l('nickel','4')= 0.009244444444442834;
profit.l = 79.34129244444443;
x.lo('nuts','1')=0;
x.lo('nuts','2')=0;
x.lo('nuts','3')=0;
x.lo('nuts','4')=0;
x.lo('bolts','1')=0;
x.lo('bolts','2')=0;
x.lo('bolts','3')=0;
x.lo('bolts','4')=0;
x.lo('washers','1')=0;
x.lo('washers','2')=0;
x.lo('washers','3')=0;
x.lo('washers','4')=0;
s.lo('iron','1')=0;
s.lo('iron','2')=0;
s.lo('iron','3')=0;
s.lo('iron','4')=0;
s.lo('iron','5')=0;
s.lo('nickel','1')=0;
s.lo('nickel','2')=0;
s.lo('nickel','3')=0;
s.lo('nickel','4')=0;
s.lo('nickel','5')=0;
s.up('iron','1')=35.80;
s.up('nickel','1')= 7.32;
execute_unload "out_var.gdx"
b
c
d
x
s
profit
x
s
profit
x
s
profit
;
$offempty
