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
test-2021-01-05 \
test-2021-01-22 \
######
testall:: all
testall:: cleanall
######
test-2021-01-05: ; \
$(XCMLTRK) \
-d test-2021-01-05.dats > test-2021-01-05.log \
######
test-2021-01-22: ; \
$(XCMLTRK) \
-d test-2021-01-22.dats > test-2021-01-22.log \
######
clean:: ; $(RMF) *~
######
cleanall:: clean
cleanall:: ; $(RMF) output
######
export XCMLTRKD="./../.."
######

###### end of [Makefile] ######
