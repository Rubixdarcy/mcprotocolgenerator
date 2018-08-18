
clean:
	rm -r out_files
	mkdir out_files

files: clean
	stack build
	stack exec mcprotocolgenerator

copy:
	cp -r out_files/* ../pluginbot/PluginBot/mcplaystate/protocol/v1_12_2/
