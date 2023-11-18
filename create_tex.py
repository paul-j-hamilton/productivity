import re
import os

def parse_bibtex(bibtex_string):
    """
    Parses a BibTex entry and returns the title, authors, and citation key.

    Parameters:
    bibtex_string (str): A string containing the BibTex entry.

    Returns:
    dict: A dictionary containing the title, authors, and citation key.
    """
    bibtex_data = {}
    lines = bibtex_string.splitlines()

    # Extract citation key
    citationKeyRE = r'{([a-z\_0-9]+),'
    match = re.search(citationKeyRE, bibtex_string)
    bibtex_data['citation_key'] = match.group(1)

    # Extract title and authors
    for line in lines:
        if line.strip().startswith("title"):
            bibtex_data['title'] = line.split('=')[1].strip('{} ,\n').replace("{", "").replace("}", "")
        elif line.strip().startswith("author"):
            authors = line.split('=')[1].strip('{} ,\n')
            authors = [author.strip() for author in authors.split(' and ')]
            # Split each author into first and last names, reverse the order, and then rejoin them with a space.
            authors = [' '.join(author.split(', ')[::-1]) for author in authors]
            # Join the formatted authors into a single string with the specified pattern.
            bibtex_data['authors'] = ', '.join(authors[:-1]) + ' \& ' + authors[-1] if len(authors) > 1 else authors[0]
    return bibtex_data

def generate_tex_file(bibtex_string, root):
    """
    Generates a .tex file from a BibTex citation.

    Parameters:
    bibtex_string (str): A string containing the BibTex entry.
    output_file (str): The path of the output .tex file.
    """
    bibtex_data = parse_bibtex(bibtex_string)

    # Prepare the content for the .tex file
    tex_content = (f"\\subsubsection*{{\\emph{{{bibtex_data['title']}}} by {bibtex_data['authors']} \\cite{{{bibtex_data['citation_key']}}}}}\n\\hrule" 
                    "\n\n\n" + bibtex)
    

    # Write to the output file
    output_file = root + "/" + bibtex_data['citation_key'] + ".tex"
    with open(output_file, 'w') as file:
        file.write(tex_content)

###################################################################################################

# Set root to be the downloads folder
wd = os.getcwd().strip("Documents\\Github\\productivity")
root = wd + "\\Downloads"

# Read in the BibTex file exported from Zotero
inputFile = root + "/Exported Items.bib"
with open(inputFile, 'r', encoding='utf-8') as file:
        bibtex = file.read()

# Generate the .tex file and write it out to Downloads
generate_tex_file(bibtex, root)