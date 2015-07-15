data map STFILEDB2-map system cat::STFILEDB2

%%
%% Datamap File PJ01AAA.SS.QSAM.CUSTOMER
%%
file PJ01AAA.SS.QSAM.CUSTOMER
organization Sequential

%%
%% Datamap File PJ01AAA.SS.QSAM.CUSTOMER.UPDATE
%%
file PJ01AAA.SS.QSAM.CUSTOMER.UPDATE
organization Sequential

%%
%% Datamap File PJ01AAA.SS.QSAM.CUSTOMER.CHECK
%%
%%file PJ01AAA.SS.QSAM.CUSTOMER.CHECK
%%organization Sequential

%%
%% Datamap File PJ01AAA.SS.QSAM.CUSTOMER.REPORT
%%
file PJ01AAA.SS.QSAM.CUSTOMER.REPORT
organization Sequential

%%
%% Datamap File PJ01AAA.SS.VSAM.CUSTOMER
%%
file PJ01AAA.SS.VSAM.CUSTOMER
  organization Indexed
  keys offset 1 bytes length 6 bytes primary
