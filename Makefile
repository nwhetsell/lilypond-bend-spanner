TAG=v0.1

all: tag archive

tag:
	git tag -a $(TAG) -m "Release $(TAG)"

archive: tag
	git archive --format=zip -o lilypond-bend-spanner-$(TAG).zip $(TAG)

