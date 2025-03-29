# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

import os

project = 'testfpm'
copyright = '2025, CA'
author = 'CA'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = []

templates_path = ['_templates']
exclude_patterns = []


# -- General configuration ---------------------------------------------------

# If your documentation needs a minimal Sphinx version, state it here.
#
# needs_sphinx = '1.0'

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'sphinx.ext.mathjax',
    'sphinx.ext.viewcode',
    'sphinx.ext.autodoc',
    'sphinx.ext.intersphinx',
    'myst_parser',
    'sphinx_rtd_theme',
    'sphinxfortran_ng.fortran_domain',
    'sphinxfortran_ng.fortran_autodoc'
]

# MyST configuration
myst_enable_extensions = [
    "dollarmath",      # Enable both block and inline math
    "amsmath",         # Adds AMS-style math features
    "deflist",         # Enable definition list syntax
    "colon_fence",     # Enable colon fences (alternative to triple backticks for code blocks)
    "html_admonition", # Admonitions like `.. note::` or `.. warning::`
    "html_image",      # Better control over image options
]

# Enable auto-generation of ToC
myst_heading_anchors = 3  # To generate heading anchors for up to level 3

# Other options
myst_number_code_blocks = ['python']  # Example: auto-number Python code blocks
myst_default_language = 'python'      # Set default language for code blocks if not specified
myst_enable_html_img = True           # Allow using HTML <img> for better image control

# MyST settings related to the table of contents
myst_toc_tree = {
    "maxdepth": 3,
    "caption": "Contents",  # This replaces `auto_toc_tree_section`
}

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# The suffix(es) of source filenames.
# You can specify multiple suffix as a list of string:
#
# source_suffix = ['.rst', '.md']
source_suffix = '.rst'

fortran_src=[
    os.path.abspath('../../src/testfpm.f90'),
    ]

#DEFAULT
fortran_ext=['f90', 'f95', 'in']

# fortran_subsection_type = "title"
# fortran_title_underline = "_"
# fortran_indent=4
# The master toctree document.
master_doc = 'index'

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'sphinx_rtd_theme'
html_static_path = ['_static']
