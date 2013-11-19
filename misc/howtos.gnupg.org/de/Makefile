all: ps dvi html txt rtf

rtf: 
	sgml2rtf --language=de --charset=latin GPGMiniHowto.sgml

dvi: 
	sgml2latex --language=de --charset=latin --output=dvi GPGMiniHowto.sgml

ps: 
	sgml2latex --language=de --charset=latin --output=ps GPGMiniHowto.sgml

html: 
	sgml2html --language=de --charset=latin GPGMiniHowto.sgml

txt: 
	sgml2txt --language=de --charset=latin GPGMiniHowto.sgml
