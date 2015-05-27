sml_sources = absyn.sml driver.sml env.sml errormsg.sml eval.sml invert.sml janus.grm janus.lex parse.sml prabsyn.sml pretty.sml prompt.lex prompt.sml sources.cm trans.sml symbol.sig symbol.sml table.sig table.sml

EXTRA_DIST = $(sml_sources) janus.grm.sig janus.grm.sml janus.lex.sml prompt.lex.sml

bin_SCRIPTS = janus@script_suffix@ janus-inv@script_suffix@ janus-trans@script_suffix@ janus-parse@script_suffix@
pkgdata_DATA = janus.@smlheapimagesuffix@ janus-inv.@smlheapimagesuffix@ janus-trans.@smlheapimagesuffix@ janus-parse.@smlheapimagesuffix@

janus@script_suffix@ :
	case @host@ in \
	*cygwin*) heapwinpath=`cygpath -w $(datadir)/$(PACKAGE)/janus.@smlheapimagesuffix@ | sed 's;\\\\;\\\\\\\\;g'` ;\
	cat `which @sml@` | sed "s;@SMLload=[^ ]*;@SMLload=$$heapwinpath;g" > janus@script_suffix@  ;; \
	*) echo "@sml@ @SMLload=$(datadir)/$(PACKAGE)/janus.@smlheapimagesuffix@ \$$*" > janus@script_suffix@  ;; \
	esac
janus.@smlheapimagesuffix@: $(sml_sources)
	echo 'CM.make "sources.cm";Driver.export_janus();' | @smlcm@

janus-inv@script_suffix@ :
	case @host@ in \
	*cygwin*) heapwinpath=`cygpath -w $(datadir)/$(PACKAGE)/janus-inv.@smlheapimagesuffix@ | sed 's;\\\\;\\\\\\\\;g'` ;\
	cat `which @sml@` | sed "s;@SMLload=[^ ]*;@SMLload=$$heapwinpath;g" > janus-inv@script_suffix@  ;; \
	*) echo "@sml@ @SMLload=$(datadir)/$(PACKAGE)/janus-inv.@smlheapimagesuffix@ \$$*" > janus-inv@script_suffix@  ;; \
	esac
janus-inv.@smlheapimagesuffix@: $(sml_sources)
	echo 'CM.make "sources.cm";Driver.export_inverter();' | @smlcm@

janus-trans@script_suffix@ :
	case @host@ in \
	*cygwin*) heapwinpath=`cygpath -w $(datadir)/$(PACKAGE)/janus-trans.@smlheapimagesuffix@ | sed 's;\\\\;\\\\\\\\;g'` ;\
	cat `which @sml@` | sed "s;@SMLload=[^ ]*;@SMLload=$$heapwinpath;g" > janus-trans@script_suffix@  ;; \
	*) echo "@sml@ @SMLload=$(datadir)/$(PACKAGE)/janus-trans.@smlheapimagesuffix@ \$$*" > janus-trans@script_suffix@  ;; \
	esac
janus-trans.@smlheapimagesuffix@: $(sml_sources)
	echo 'CM.make "sources.cm";Driver.export_trans();' | @smlcm@

janus-parse@script_suffix@ :
	case @host@ in \
	*cygwin*) heapwinpath=`cygpath -w $(datadir)/$(PACKAGE)/janus-parse.@smlheapimagesuffix@ | sed 's;\\\\;\\\\\\\\;g'` ;\
	cat `which @sml@` | sed "s;@SMLload=[^ ]*;@SMLload=$$heapwinpath;g" > janus-parse@script_suffix@  ;; \
	*) echo "@sml@ @SMLload=$(datadir)/$(PACKAGE)/janus-parse.@smlheapimagesuffix@ \$$*" > janus-parse@script_suffix@  ;; \
	esac
janus-parse.@smlheapimagesuffix@: $(sml_sources)
	echo 'CM.make "sources.cm";Driver.export_parse();' | @smlcm@

clean-local:
	rm -rf CM
	rm -f $(bin_SCRIPTS) $(pkgdata_DATA)
