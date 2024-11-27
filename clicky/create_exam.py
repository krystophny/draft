import sys
import pyautogui
from pathlib import Path
from time import sleep
from datetime import date

base_path = Path('/Users/ert/Nextcloud/clicky')


def main():
    lecture_name = sys.argv[1]
    if len(sys.argv) > 2:
        participant = sys.argv[2]
    else:
        participant = None
    if len(sys.argv) > 3:
        exam_date = sys.argv[3]
    else:
        exam_date = today()

    click_button('manage_exams')
    sleep(0.5)
    create_exam()
    sleep(1.0)
    click_button('new_exam')
    sleep(0.5)
    select_lecture()
    sleep(2.0)
    select_all()
    delete_text()
    pyautogui.write(lecture_name + "*")
    pyautogui.press('enter')
    sleep(1.0)
    activate_lecture_radiobox()
    sleep(0.1)
    confirm_lecture()
    sleep(0.5)
    click_button('exam_date')
    sleep(0.1)
    pyautogui.write(exam_date)
    pyautogui.press('tab', 4)
    pyautogui.press('down', 4)
    pyautogui.press('enter')
    pyautogui.press('tab', 21)
    pyautogui.press('enter')
    sleep(0.5)
    if(participant):
        register_participant(participant)


def register_participant(participant):
    select_participants()
    sleep(0.5)
    click_button('register')
    sleep(0.5)
    pyautogui.write(participant)
    pyautogui.press('enter')


def select_participants():
    pyautogui.press('tab', 2)
    pyautogui.press('enter')


def create_exam():
    pyautogui.press('tab', 9)
    pyautogui.press('enter')


def select_lecture():
    pyautogui.press('tab', 1)
    pyautogui.press('enter')


def confirm_lecture():
    pyautogui.press('tab', 3)
    pyautogui.press('enter')


def activate_lecture_radiobox():
    pyautogui.press('tab', 4)
    pyautogui.press('space')


def delete_text():
    pyautogui.press('backspace')


def select_all():
    pyautogui.hotkey('command', 'a')


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


def today():
    return date.today().strftime('%d.%m.%Y')


if __name__ == '__main__':
    main()
