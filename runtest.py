import glob, os, sys

inPath = "test/in/"
outPath = "test/out/"

def run_test(testName):
	cmd = "cat "+inPath+testName+".in | runhaskell Interpret.hs"
	result = os.popen(cmd, "r").read() 
	try:
		out = open(os.path.join(outPath, testName + ".out")).read()
		if result == out:
			print "OK: " + testName
		else:
			print "ERROR: " + testName
			print result
	except Exception as e:
		print "PYTHON ERROR"
		print str(e)
		print result


def run_tests():	
	print "Runing all tests:"
	for infile in sorted(os.listdir(inPath)):
		testName = infile.split(".")[0]
		run_test(testName)
		

if __name__ == "__main__":
	if len(sys.argv) == 1: run_tests()
	else: run_test(sys.argv[1])


