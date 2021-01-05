######
#
# For testing xcmltrk
#
######
CAT=cat
CPF=cp -f
RMF=rm -f
ECHO=echo
######
XATSOPT=\
"$(XATSHOME)/bin/xatsopt"
######
all:: \
test-2021-01-05
######
testall:: all
testall:: cleanall
######
test-2021-01-05: ; \
$(XATSOPT) \
-d test-2021-01-15.dats
######
clean:: ; $(RMF) *~
######
cleanall:: clean
cleanall:: ; $(RMF) output
######
export XATSOPTD="./../../xatsopt"
######

###### end of [Makefile] ######
