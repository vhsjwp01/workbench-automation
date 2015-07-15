ufas mapper STFILEORA

%%
%% Desc file PJ01AAA.SS.QSAM.CUSTOMER
%%
file PJ01AAA.SS.QSAM.CUSTOMER transferred
include "COPY/ODCSF0.cpy"
map record QS-ODCSF0-RECORD defined in "COPY/ODCSF0.cpy"
source record QS-ODCSF0-RECORD defined in "COPY/ODCSF0.cpy"
logical name ODCSF0Q
converter name ODCSF0Q

%%
%% Desc file PJ01AAA.SS.QSAM.CUSTOMER.UPDATE
%%
file PJ01AAA.SS.QSAM.CUSTOMER.UPDATE transferred
include "COPY/ODCSFU.cpy"
map record QS-ODCSF0-RECORD defined in "COPY/ODCSFU.cpy"
source record QS-ODCSF0-RECORD defined in "COPY/ODCSFU.cpy"
logical name ODCSFU
converter name ODCSFU

%%
%% Desc file PJ01AAA.SS.QSAM.CUSTOMER.CHECK
%%
%%Duplicate Copie file PJ01AAA.SS.QSAM.CUSTOMER.CHECK transferred
%%Duplicate Copie include "COPY/ODCSF0.cpy"
%%Duplicate Copie map record QS-ODCSF0-RECORD defined in "COPY/ODCSF0.cpy"
%%Duplicate Copie source record QS-ODCSF0-RECORD defined in "COPY/ODCSF0.cpy"
%%Duplicate Copie logical name ODCSF0
%%Duplicate Copie converter name ODCSF0

%%
%% Desc file PJ01AAA.SS.QSAM.CUSTOMER.REPORT
%%
file PJ01AAA.SS.QSAM.CUSTOMER.REPORT transferred
include "COPY/MW_SYSOUT.cpy"
map record MW-SYSOUT defined in "COPY/MW_SYSOUT.cpy"
source record MW-SYSOUT defined in "COPY/MW_SYSOUT.cpy"
logical name MW_SYSOUT
converter name MW_SYSOUT

%%
%% Desc file PJ01AAA.SS.VSAM.CUSTOMER
%%
file PJ01AAA.SS.VSAM.CUSTOMER transferred converted
   table name CUSTOMER
   include "COPY/ODCSF0B.cpy"
   map record VS-ODCSF0-RECORD defined in "COPY/ODCSF0B.cpy"
   source record VS-ODCSF0-RECORD defined in "COPY/ODCSF0B.cpy"
   logical name ODCSF0 %% Using assign Name with the same Cics usage
   converter name ODCSF0
