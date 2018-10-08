#!/usr/bin/env python3


def main():
    import alsaaudio
    m = alsaaudio.Mixer()
    volume = m.getvolume()[0]
    mute = m.getmute()
    if mute == 1:
        mute = ' Muted'
    else:
        mute = ''
    print(f'Volume: {volume}%{mute}')


if __name__ == '__main__':
    try:
        main()
    except:
        print('Volume Error')
