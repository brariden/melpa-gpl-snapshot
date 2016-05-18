//
// Copyright (c) 2015 Lorenzo Villani
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//

package main

import (
	"bytes"
	"flag"
	"io"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/ungerik/go-dry"
)

//
// Formatters
//

const EMACS = "emacs"
const SUBLIME = "sublime"

var editors = []string{EMACS, SUBLIME}

type installMap map[string][]string
type syntaxMap map[string][]string

type formatter struct {
	Commands   [][]string
	Extensions []string
	Install    installMap
	Syntaxes   syntaxMap
}

var formatters = []*formatter{
	// C/C++
	{
		Commands: [][]string{
			[]string{"clang-format", "-style=WebKit", "-"},
		},
		Extensions: []string{".c", ".cpp", ".cxx", ".h", ".hpp", ".hxx"},
		Install: installMap{
			"darwin": []string{"brew", "install", "clang-format"},
		},
		Syntaxes: syntaxMap{
			EMACS:   []string{"c-mode", "c++-mode"},
			SUBLIME: []string{"C"},
		},
	},
	// CSS
	{
		Commands: [][]string{
			[]string{"cssfmt"},
		},
		Extensions: []string{".css"},
		Install: installMap{
			"any": []string{"npm", "install", "-g", "cssfmt"},
		},
		Syntaxes: syntaxMap{
			EMACS:   []string{"css-mode"},
			SUBLIME: []string{"CSS"},
		},
	},
	// Go
	{
		Commands: [][]string{
			[]string{"goimports"},
		},
		Extensions: []string{".go"},
		Install: installMap{
			"any": []string{"go", "get", "golang.org/x/tools/cmd/goimports"},
		},
		Syntaxes: syntaxMap{
			EMACS:   []string{"go-mode"},
			SUBLIME: []string{"GoSublime-Go"},
		},
	},
	// JavaScript
	{
		Commands: [][]string{
			[]string{"semistandard-format", "-"},
		},
		Extensions: []string{".js", ".jsx"},
		Install: installMap{
			"any": []string{"npm", "install", "-g", "semistandard-format"},
		},
		Syntaxes: syntaxMap{
			EMACS:   []string{"js-mode", "js2-mode", "js3-mode"},
			SUBLIME: []string{"JavaScript", "JavaScript (Babel)"},
		},
	},
	// JSON
	{
		Commands: [][]string{
			[]string{"jsonlint", "--sort-keys", "-"},
		},
		Extensions: []string{".json"},
		Install: installMap{
			"any": []string{"npm", "install", "-g", "jsonlint"},
		},
		Syntaxes: syntaxMap{
			EMACS:   []string{"json-mode"},
			SUBLIME: []string{"JSON", "Sublime Commands"},
		},
	},
	// Python
	{
		Commands: [][]string{
			[]string{"autopep8", "--max-line-length=98", "-"},
			[]string{"isort", "--line-width", "98", "--multi_line", "3", "-"},
		},
		Install: installMap{
			"any": []string{"pip", "install", "autopep8", "isort"},
		},
		Extensions: []string{".py"},
		Syntaxes: syntaxMap{
			EMACS:   []string{"python-mode"},
			SUBLIME: []string{"Python"},
		},
	},
	// SASS
	{
		Commands: [][]string{
			[]string{"sass-convert", "--no-cache", "--from", "sass", "--to", "sass", "--indent", "4", "--stdin"},
		},
		Install: installMap{
			"any": []string{"gem", "install", "sass"},
		},
		Extensions: []string{".sass"},
		Syntaxes: syntaxMap{
			EMACS:   []string{"sass-mode"},
			SUBLIME: []string{"SASS"},
		},
	},
	// SCSS
	{
		Commands: [][]string{
			[]string{"sass-convert", "--no-cache", "--from", "scss", "--to", "scss", "--indent", "4", "--stdin"},
		},
		Extensions: []string{".scss"},
		Install: installMap{
			"any": []string{"gem", "install", "sass"},
		},
		Syntaxes: syntaxMap{
			EMACS:   []string{"scss-mode"},
			SUBLIME: []string{"SCSS"},
		},
	},
}

//
// Lookup maps
//

var extToFormatter = make(map[string]*formatter)
var syntaxToFormatter = make(map[string]map[string]*formatter)

func init() {
	for _, editor := range editors {
		syntaxToFormatter[editor] = make(map[string]*formatter)
	}

	for _, formatter := range formatters {
		for _, ext := range formatter.Extensions {
			extToFormatter[ext] = formatter
		}

		for editor, syntaxes := range formatter.Syntaxes {
			for _, syntax := range syntaxes {
				syntaxToFormatter[editor][syntax] = formatter
			}
		}
	}
}

func formatterForPath(path string) *formatter {
	ext := filepath.Ext(path)
	if ext == "" {
		return nil
	}

	fmt, ok := extToFormatter[ext]
	if !ok {
		return nil
	}

	return fmt
}

//
// Flags
//

var editor = flag.String("editor", "", "Editor name")
var ignoreErrors = flag.Bool("ignore-errors", false, "Ignore formatter errors")
var install = flag.Bool("install", false, "Install formatters")
var syntax = flag.String("syntax", "", "Editor syntax name")
var verbose = flag.Bool("verbose", false, "Verbose output")
var write = flag.Bool("write", false, "Write the file in place")

//
// Entry point
//

type formatOp func(string, *formatter) error

func main() {
	// Flags
	flag.Parse()

	if *install {
		installFormatters()
	}

	args := flag.Args()
	if len(args) < 1 {
		return
	}

	// Format standard input, then stop
	if len(args) == 1 && args[0] == "-" {
		formatStdin()
		return
	}

	// Select mode of operation (format to file or standard output)
	var op formatOp
	if *write {
		op = formatWrite
	} else {
		op = formatStdout
	}

	// Format files
	for _, path := range args {
		if dry.FileIsDir(path) {
			formatDir(path, op)
		} else {
			formatFile(path, op)
		}
	}
}

//
// High level operations
//

func installFormatters() {
	platforms := []string{"any", runtime.GOOS}

	for _, formatter := range formatters {
		for _, platform := range platforms {
			cmd, ok := formatter.Install[platform]
			if ok {
				c := exec.Command(cmd[0], cmd[1:]...)

				log.Println(strings.Join(cmd, " "))

				if err := c.Run(); err != nil {
					log.Fatalln(err)
				}

				break
			}
		}
	}
}

var ignoreDirs = []string{".git", ".hg", ".svn", "node_modules"}

func formatDir(path string, op formatOp) {
	filepath.Walk(path, func(path string, info os.FileInfo, err error) error {
		if info.IsDir() && dry.StringListContains(ignoreDirs, info.Name()) {
			return filepath.SkipDir
		}

		if !info.IsDir() {
			formatFile(path, op)
		}

		return nil
	})
}

func formatFile(path string, op formatOp) {
	formatter := formatterForPath(path)
	if formatter == nil {
		return
	}

	if *verbose {
		log.Println("Formatting", path)
	}

	if err := op(path, formatter); err != nil {
		if *ignoreErrors {
			log.Println(err, "(ignored)")
		} else {
			log.Fatalln(err)
		}
	}
}

func formatStdin() {
	if *editor == "" && *syntax == "" {
		log.Fatalln("I need to know the editor and syntax combo")
	}

	formatter, ok := syntaxToFormatter[*editor][*syntax]
	if !ok {
		log.Fatalln("Cannot find a supported formatter for given editor + syntax combo")
	}

	if err := formatChain(os.Stdout, os.Stdin, formatter.Commands); err != nil {
		log.Fatalln(err)
	}
}

//
// Low level operations
//

func formatWrite(path string, formatter *formatter) error {
	file, err := os.OpenFile(path, os.O_RDWR, 0)
	if err != nil {
		return err
	}
	defer file.Close()

	var buf bytes.Buffer

	if err := formatChain(&buf, file, formatter.Commands); err != nil {
		return err
	}

	if err := file.Truncate(0); err != nil {
		return err
	}

	if _, err := file.Seek(0, os.SEEK_SET); err != nil {
		return err
	}

	_, err = io.Copy(file, &buf)
	return err
}

func formatStdout(path string, formatter *formatter) error {
	file, err := os.Open(path)
	if err != nil {
		return err
	}
	defer file.Close()

	return formatChain(os.Stdout, file, formatter.Commands)
}

func formatChain(dst io.Writer, src io.Reader, commandChain [][]string) error {
	var buf, tmp bytes.Buffer

	for i, command := range commandChain {
		var stepSrc io.Reader

		if i == 0 {
			stepSrc = src
		} else {
			tmp.Reset()

			if _, err := io.Copy(&tmp, &buf); err != nil {
				return err
			}

			buf.Reset()

			stepSrc = &tmp
		}

		if err := format(&buf, stepSrc, command); err != nil {
			return err
		}
	}

	_, err := io.Copy(dst, &buf)
	return err
}

func format(dst io.Writer, src io.Reader, command []string) error {
	cmd := exec.Command(command[0], command[1:]...)
	cmd.Stdin = src
	cmd.Stdout = dst

	if err := cmd.Run(); err != nil {
		return err
	}

	return nil
}
