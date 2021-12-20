package main

import (
	"bytes"
	"fmt"
	"log"
	"os"
	"io"
	"os/exec"
	"strings"
	"bufio"
)

func ex1() {
	////////////////////////////////////////////////////////////////////////////////
	// run an external program, get it's stdout
	out, err := exec.Command("date").Output()
	if err != nil {
		log.Fatal(err)
		panic(err)
	}
	fmt.Printf("The date is %q\n", out)
}

func ex2() {
	////////////////////////////////////////////////////////////////////////////////
	// failing example
	out, err := exec.Command("xxdate").Output()
	if err != nil {
		fmt.Printf("Error executing command: %q\n", err)
	}
	fmt.Printf("The date is %q\n", out)

}

func ex3() {
	////////////////////////////////////////////////////////////////////////////////
	// run an external program, write both stdout and stderr to a buffer, take
	// stin from a buffer
	proc := exec.Command("ruby", "-e", "$stdout.sync = true; @n = $stdin.readline.to_i; @n.times {|ii| puts ii; sleep ii }; $stderr.puts 'on stderr'")
	proc.Stdin = strings.NewReader("3\n")
	var procout, procerr bytes.Buffer
	proc.Stdout = &procout
	proc.Stderr = &procerr
	err := proc.Run()
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("output of cmd is: stdout=%s stderr=%s\n", procout.String(), procerr.String())
}

func ex4() {
	////////////////////////////////////////////////////////////////////////////////
	// use pipes to get a stream for stdout/stderr
	proc := exec.Command("ruby", "-e", "$stdout.sync = true; @n = $stdin.readline.to_i; @n.times {|ii| puts ii; sleep ii }; $stderr.puts 'on stderr'")
	stdout, err := proc.StdoutPipe()
	if err != nil {
		fmt.Printf("Error getting StdoutPipe: %q\n", err)
	}

	stderr, err := proc.StderrPipe()
	if err != nil {
		fmt.Printf("Error getting StderrPipe: %q\n", err)
	}

	stdin, err := proc.StdinPipe()
	if err != nil {
		fmt.Printf("Error getting StdinPipe: %q\n", err)
	}

	err = proc.Start()
	if err != nil {
		log.Fatal(err)
	}

	go func() {
		fmt.Printf("golang: about to read from stdout\n")
		os.Stdout.Sync()
    lineReader := bufio.NewReader(stdout)
		for {
			line, err := lineReader.ReadString('\n')
      if err == io.EOF {
			  fmt.Printf("golang: stdout='%s'\n", line)
				fmt.Printf("Hit io.EOF on stdout: %q\n", io.EOF)
				break
      }
			if err != nil {
				fmt.Printf("Error reading from stdout: %q\n", err)
				break
			}
			fmt.Printf("golang: stdout='%s'\n", line)
		}
	}()

	go func() {
		//var buff = make([]byte, 1024)
		fmt.Printf("golang: about to read from stderr\n")
		os.Stdout.Sync()
    lineReader := bufio.NewReader(stderr)
		for {
			line, err := lineReader.ReadString('\n')
      if err == io.EOF {
			  fmt.Printf("golang: stderr='%s'\n", line)
				fmt.Printf("Hit io.EOF on stderr: %q\n", io.EOF)
				break
      }
			if err != nil {
				fmt.Printf("Error reading from stderr: %q\n", err)
				break
			}
			fmt.Printf("golang: stderr='%s'\n", line)
		}
	}()

	go func() {
		fmt.Printf("golang: about to write to stdin\n")
		os.Stdout.Sync()
		num, err := stdin.Write([]byte("2\n"))
		if err != nil {
			fmt.Printf("Error reading from stderr: %q\n", err)
			return
		}
		fmt.Printf("golang: %d bytes written to stdin\n", num)
	}()

	proc.Wait()
	fmt.Printf("proc has exited")
}

func main() {
	//ex1()
	//ex2()
	//ex3()
	ex4()
}
