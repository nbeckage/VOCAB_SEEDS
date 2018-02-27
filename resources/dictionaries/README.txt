Notation:
noSW: without stopwords (functional words).
Gkh: K-hierarchy (see paper).
Gk-scch: C-hierarchy (see paper).
Stem: words were stemmatized before processing the graphs.
Lemma: words were lemmatized before processing the graphs.

Format:
The format of every file is the following:
	-1st column: Word (stem or lemma).
	-2nd colum: Part of speech.
	-3rd column: 
		-For basic files (ends with lemma, lemma_noSW, stem, stem_SW):
			-1: Core
			-2: Satellites
			-3: Rest
		-For hierarchy files (ends with gkh or gk-scch):
			-0: Core or Kernel (based on the type or hierarchy):
			-1+: Definitional Distance from Kernel or Definitional Distance from Core within Kernel.