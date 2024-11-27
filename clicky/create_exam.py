import sys
import pyautogui
from pathlib import Path
from time import sleep

base_path = Path('/Users/ert/Nextcloud/clicky')


def main():
    lecture_name = sys.argv[1]
    click_button('manage_exams')
    sleep(0.5)
    click_button('create_exam')
    sleep(0.5)
    click_button('new_exam')
    sleep(0.5)
    click_button('exam_lecture')
    sleep(4)
    type_text(lecture_name)


def type_text(text):
    pyautogui.write(text, interval=0.01)
    pyautogui.press('enter')


def click_button(name):
    picture = (base_path / f'{name}.png').as_posix()
    button = pyautogui.locateOnScreen(picture, confidence=0.8)
    center = transform(get_center(button))
    pyautogui.click(center[0], center[1], clicks=2, button='left')


def transform(pos):
    """Required on retina displays"""
    return pos[0]*0.5, pos[1]*0.5


def get_center(rect):
    return (rect.left + rect.width*0.5, rect.top + rect.height*0.5)


if __name__ == '__main__':
    main()
