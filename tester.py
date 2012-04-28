import glob, os

inPath = "test/in/"
outPath = "test/out/"

def run_tests():	
	print "Runing all tests:"
	for infile in sorted(os.listdir(inPath)):
		testName = infile.split(".")[0]
		cmd = "cat "+inPath+infile+" | runhaskell Interpret.hs"
		result = os.popen(cmd, "r").read() 
		out = open(os.path.join(outPath, testName + ".out")).read()
		if result == out:
			print "OK: " + testName
		else:
			print "ERROR: " + testName

if __name__ == "__main__":
	run_tests()


