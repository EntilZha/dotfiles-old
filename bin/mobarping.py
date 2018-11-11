#!/usr/bin/env python3
import subprocess
import re


COMMAND = 'ping -i .2 -c 1 google.com'
PATTERN = r'64.*time=([0-9\.]+)\s+ms'


def main():
    result = subprocess.run(COMMAND, shell=True, stdout=subprocess.PIPE)
    ping_data = result.stdout.decode('utf8').split('\n')
    for line in ping_data:
        m = re.match(PATTERN, line)
        if m is not None:
            ping = m.group(1)
            print(f'{ping:>4s}ms')
            return
    print('unreachable')


if __name__ == '__main__':
    main()
