
# all: ../lib/css/bootstrap.css ../lib/css/bootstrap-print.css ../lib/css/tinymce-zotonic.css
# all: ../lib/css/bootstrap.css ../lib/css/tinymce-zotonic.css fonts
all: ../lib/css/site.css ../lib/css/tinymce-zotonic.css

../lib/css/site.css: bootstrap-sass-3.4.1/assets/stylesheets/* \
	bootstrap-sass-3.4.1/assets/stylesheets/bootstrap/* \
	*.scss \
	site/* \
	tufte/*
	sass bootstrap.scss ../lib/css/site.css

../lib/css/bootstrap-print.css: bootstrap-sass-3.4.1/assets/stylesheets/* \
	bootstrap-sass-3.4.1/assets/stylesheets/bootstrap/* \
	*.scss \
	site/* \
	tufte/*
	sass bootstrap-print.scss ../lib/css/bootstrap-print.css

../lib/css/tinymce-zotonic.css: tinymce-zotonic.scss
	sass tinymce-zotonic.scss ../lib/css/tinymce-zotonic.css

fontsdir := ../lib/fonts
fontsrcs := $(wildcard bootstrap-sass-3.4.1/assets/fonts/bootstrap/glyphicons-halflings-regular.*)
glyphicons := $(subst bootstrap-sass-3.4.1/assets/fonts/bootstrap, $(fontsdir), $(fontsrcs))

.PHONY: fonts
fonts: $(glyphicons)

$(fontsdir)/glyphicons-halflings-regular.%: bootstrap-sass-3.4.1/assets/fonts/bootstrap/glyphicons-halflings-regular.%
	cp $< ../lib/fonts/
