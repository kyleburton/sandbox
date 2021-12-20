import cmd, sys, os # NOQA


class MyShell(cmd.Cmd):
    intro = "Example CLI Shell"
    prompt = 'tfshell you@YourAwsAccount> '
    file = None

    def do_ls(self, arg):
        os.system("ls {}".format(arg))

    def do_exit(self, arg):
        # return True, the shell exits
        return True


def parse(arg):
    return tuple(arg.split())

if __name__ == '__main__':
    MyShell().cmdloop()
