ufas mapper STDB2ORA

%%
%% Desc file PJ01AAA.S2.QSAM.CUSTOMER
%%
file PJ01AAA.S2.QSAM.CUSTOMER transferred
include "COPY/ODCSF0.cpy"
map record QS-ODCSF0-RECORD defined in "COPY/ODCSF0.cpy"
source record QS-ODCSF0-RECORD defined in "COPY/ODCSF0.cpy"
logical name ODCSF0
converter name ODCSF0

%%
%% Desc file PJ01AAA.S2.QSAM.CUSTOMER.UPDATE
%%
file PJ01AAA.S2.QSAM.CUSTOMER.UPDATE transferred
include "COPY/ODCSFU.cpy"
map record QS-ODCSF0-RECORD defined in "COPY/ODCSFU.cpy"
source record QS-ODCSF0-RECORD defined in "COPY/ODCSFU.cpy"
logical name ODCSFU
converter name ODCSFU

