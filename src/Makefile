ERLC= erlc
ERLC_FLAGS=-pa /usr/local/jungerl/jungerl/lib/xmerl/ebin/ -I../include 
EBIN=../ebin

OBJS = \
	$(EBIN)/mime.beam \
	$(EBIN)/mime_types.beam \
	$(EBIN)/mime_gen.beam \
	$(EBIN)/em2w.beam \
	$(EBIN)/em2w_verify.beam \
	$(EBIN)/em2w_config.beam

all:	$(OBJS)

clean:
	rm $(OBJS)

$(EBIN)/em2w.beam:		em2w.hrl


$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERLC_FLAGS) -o$(EBIN) $<
