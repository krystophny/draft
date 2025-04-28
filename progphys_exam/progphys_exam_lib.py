import numpy as np

def questions_by_category(data, category):
    filtered_questions = []
    for i in range(len(data)):
        if data[i, 1] == category:
            filtered_questions.append(data[i, :])
    return np.array(filtered_questions)

def questions_by_category_fast(data, category):
    return data[data[:, 1] == category]

def generate_exam_sheet(questions, category_per_sheet=[1, 1, 1]):
    for category, question in questions.items():
        question_number = np.random.randint(0, question.shape[0])
        print(category, question[question_number, 0])
