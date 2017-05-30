all:
	ghc --make -o tsoutapl Main.hs

test:
	./tsoutapl untyped --verbose sample_untyped.prg \
		&& ./tsoutapl typed --verbose sample_typed.prg \
		&& ./tsoutapl typed --verbose --context sample_typed.ctx sample_typed.prg
