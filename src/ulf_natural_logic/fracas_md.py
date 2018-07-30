#!/usr/bin/env python3
#-*- coding:utf-8 -*-

# Performs operations on the FraCaS .md files.
# See  fracas_md.py --help  for details.

import os, sys, argparse
from collections import OrderedDict

class Markdown(OrderedDict):
	def __init__(self, filename=None):
		OrderedDict.__init__(self)
		if filename: self.parse(filename)
	#enddef

	def parse(self, filename):
		# Just a very lame parsers for our uses.
		# Expects headers and at most a code block followed by a paragraph.
		self.clear()
		self.filename = filename

		parents = [self]
		curDepth = 0
		curSection = self

		pstart = False

		for line in open(filename, "r"):
			sline = line.strip()

			if sline.startswith("#"):
				# Section header.
				for i, x in enumerate(sline):
					if x != "#": break
				#endfor

				if i <= curDepth:
					# This was a lower depth, so back up to whereever's appropriate.
					# NOTE: this assumes no depths were skipped.
					parents = parents[:i]
					curSection = parents[-1]
				#endif

				header = sline[i:].rstrip("#").strip()
				curSection[header] = curSection = Markdown()
				parents.append(curSection)

				pstart = False
				curDepth = i

			elif line.startswith("    ") or line.startswith("\t"):
				# Code block.
				start = 1 if line.startswith("\t") else 4
				curSection.setdefault("code", []).append(line[start:].rstrip())
			elif sline or pstart:
				# Paragraph.
				pstart = True
				curSection.setdefault("p", []).append(line.rstrip("\r\n"))
			#endif
		#endfor

		self.sanitize()
	#enddef

	def sanitize(self):
		for k, v in self.items():
			if k == "code":
				# The code section is expected to start with a sentence and
				# then be followed by an annotation. We can know annotations
				# start with a ( and should have balanced parens but may not..
				# We can probably assume sentences start at 0 with a non-(
				sentences = []
				sentence = ""
				annotation = ""
				for line in v:
					if line[0] in "( \t":
						annotation += " " + line.lstrip()
					elif sentence:
						sentences.append((sentence, annotation.strip()))
						sentence = line
						annotation = ""
					else:
						sentence = line
					#endif
				#endfor

				# Add the last sentence.
				if sentence:
					sentences.append((sentence, annotation.strip()))
				#endif

				final = OrderedDict()
				hypothesis = sentences.pop()

				for i, pair in enumerate(sentences):
					final["P%i" % (i+1)] = pair
				#endfor

				final["H"] = hypothesis

				self[k] = final

			elif k == "p":
				p = ""

				for line in v:
					if line.endswith("  "):
						p += line[:-2] + "\n"
					elif line.strip():
						p += line
					else:
						p += "\n"
					#endif
				#endfor

				self[k] = p

			else:
				v.sanitize()
			#endif
		#endfor
	#enddef
#endclass

def main(args):
	parser = argparse.ArgumentParser(
		description = "Run various tasks over the FraCaS markdown files.")
	parser.add_argument("-?", #/?, /h
		dest    = "help",
		help    = argparse.SUPPRESS,
		action  = "store_true")
	parser.add_argument("--stats", "-s",
		help    = "Print some statistics about the file(s).",
		action  = "store_true")
	parser.add_argument("--export", "-x",
		help    = "Export the annotations to the given folder as one sentence per file.",
		metavar = ("folder",),
		default = None)
	parser.add_argument("--reconstruct", "-r",
		help    = "Attempt to reconstruct the sentence from the annotation to compare to the given sentence.",
		action  = "store_true")
	parser.add_argument("files",
		help    = "Markdown files to work on.",
		nargs   = "*")

	args = parser.parse_args(args)

	if args.help or "/?" in args.files or "/h" in args.files:
		parser.print_help()
		return 0
	#endif

	# Load all the markdown files.
	mds = [Markdown(f) for f in args.files]

	if args.stats:
		for md in mds:
			# Assume one top-level with sublevels for each test.
			top = tuple(md.values())[0]

			total = 0
			completed = 0
			annotated = 0

			for data in top.values():
				incomplete = 0
				for _, annotation in data.get("code", {}).values():
					if annotation and annotation != "()" and annotation.count("(") == annotation.count(")"):
						annotated += 1
					else:
						incomplete += 1
					#endif
				#endfor

				todo = "TODO" in data.get("p", "")

				if not (incomplete or todo):
					completed += 1
				#endif

				total += 1
			#endfor

			print("Stats for", md.filename)
			print("  Completed %.2f%% of entries with %i total annotations." % (completed / total * 100, annotated))
		#endfor

	elif args.export:
		for md in mds:
			folder = os.path.join(args.export, md.filename.replace(".md", ""))
			os.makedirs(folder, exist_ok=True)

			# Assume one top-level with sublevels for each test.
			# Each sublevel is named as the test name (fracas-###).
			# Each test contains some number of P# entries and a H entry.
			top = tuple(md.values())[0]
			for name, data in top.items():
				if "TODO" in data.get("p", ""):
					# This isn't done, and we don't know which.
					continue
				#endif

				for label, (_, annotation) in data.get("code", {}).items():
					if annotation and annotation != "()" and annotation.count("(") == annotation.count(")"):
						filename = os.path.join(folder, name + "_" + label + ".txt")
						with open(filename, "w") as f:
							f.write(annotation)
						#endwith
					#endif
				#endfor
			#endfor
		#endfor

	elif args.reconstruct:
		# Basically just chop out any .xxs, collapse parens, deal with plur/tenses...
		print("Not implemented.")
	#endif

	return 0
#enddef

if __name__ == "__main__":
	try: sys.exit(main(sys.argv[1:]))
	except (EOFError, KeyboardInterrupt):
		if debug: raise
		print("\nOperations terminated by user.")
		sys.exit(0)
	#endtry
#endif
