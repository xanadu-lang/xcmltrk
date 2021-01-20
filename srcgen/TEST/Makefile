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
XCMLTRK=$(XCMLTRKD)/bin/xcmltrk
######
all:: \
test-2021-01-05
######
testall:: all
testall:: cleanall
######
test-2021-01-05: ; \
$(XCMLTRK) \
-d test-2021-01-05.dats
######
test1: ; \
$(XCMLTRK) \
-d test1.dats
######
clean:: ; $(RMF) *~
######
cleanall:: clean
cleanall:: ; $(RMF) output
######
export XCMLTRKD="./../.."
######

###### end of [Makefile] ######
