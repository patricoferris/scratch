pta-claim.json : private-to-audit.lurk
	fcomm eval --expression private-to-audit.lurk --claim pta-claim.json --lurk

pr1.lurk:
	lurk_of_json/_build/default/main.exe private data/pr1.json > pr1.lurk

pta-open-proof-pr1.json: private-to-audit.lurk pta-function.json pr1.lurk
	fcomm open --function pta-function.json --input pr1.lurk --proof pta-open-proof-pr1.json

verify-pta-open-proof-pr1: pta-open-proof-pr1.json
	fcomm verify --proof pta-open-proof-pr1.json

.PHONY : clean
clean :
	rm pr1.lurk pta-open-proof-pr1.json