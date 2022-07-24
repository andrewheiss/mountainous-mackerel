remote_host = cloud
remote_dir = ~/sites/stats/public_html/mountainous-mackerel
remote_dest = $(remote_host):$(remote_dir)

.PHONY: build upload serve

serve:
	quarto preview analysis

build:
	quarto render analysis

upload:
	rsync -crvP --delete analysis/_site/ $(remote_dest)
