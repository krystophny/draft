# %%
import numpy as np
from progphys_exam_lib import questions_by_category, generate_exam_sheet

filename = "questions_with_category_and_exercise.csv"
data = np.loadtxt(filename, delimiter=";", dtype=str, skiprows=1)
categories = ["a", "b", "c"]
num_students = 10

questions = {}
for c in categories:
    questions[c] = questions_by_category(data, c)

for k in range(num_students):
    print(f"Student {k}")
    generate_exam_sheet(questions=questions, category_per_sheet=[1, 1, 1])
    print()

# %%
