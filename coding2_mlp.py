import pandas as pd
from sklearn.preprocessing import OrdinalEncoder, StandardScaler, OneHotEncoder
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
from sklearn.impute import SimpleImputer
from sklearn.model_selection import train_test_split
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import classification_report, accuracy_score

'''
v0: 18 + 8
v1: 17 + 5
v3: 15 + 1
v4: 14 + 3
'''
df = pd.read_csv("./v4_Vehicle Coupon Recommendation.csv")

X = df.drop('Y', axis=1)
y = df['Y']  # 目标变量Y

# 预处理
cat_features = X.select_dtypes(include=["object"]).columns.tolist()
num_features = X.select_dtypes(exclude=["object"]).columns.tolist()
print(cat_features, num_features)

# ordinal_features = ['time', 'expiration', 'age', 'education', 'income', 'Restaurant20To50']
# onehot_features = [f for f in cat_features if f not in ordinal_features]

# time_order = ['7AM', '10AM', '2PM', '6PM', '10PM']
# expiration_order = ['2h', '1d']
# age_order = ['below21', '21', '26', '31', '36', '41', '46', '50plus']
# # age_order = ['below21', '20-30', '30-40', '40-50', '50plus']
# education_order = ['Some High School', 'High School Graduate', 'Some college - no degree', 'Associates degree', 'Bachelors degree','Graduate degree (Masters or Doctorate)']
# # income_order = ['Low', 'Medium', 'High']
# income_order = ['Less than $12500', '$12500 - $24999', '$25000 - $37499', '$37500 - $49999', '$50000 - $62499',  '$62500 - $74999', '$75000 - $87499', '$87500 - $99999','$100000 or More']
# Restaurant20To50_order = ['gt8', '4~8', '1~3', 'less1', 'never', 'missing']

numerical_transformer = Pipeline(steps=[
    ('imputer', SimpleImputer(strategy='median')),
    ('scaler', StandardScaler())
])
categorical_transformer = Pipeline(steps=[
    ('imputer', SimpleImputer(strategy='constant', fill_value='missing')),
    ('onehot', OneHotEncoder(handle_unknown='ignore'))
])
# ordinal_transformer = Pipeline(steps=[
#     ('imputer', SimpleImputer(strategy='constant', fill_value='missing')),
#     ('ordinal', OrdinalEncoder(categories=[time_order, expiration_order, age_order, education_order, income_order, Restaurant20To50_order]))
# ])
preprocessor = ColumnTransformer(
    transformers=[
        ('num', numerical_transformer, num_features),
        ('cat', categorical_transformer, cat_features),
        # ('cat', categorical_transformer, onehot_features),
        # ('ordinal', ordinal_transformer, ordinal_features)
    ])
X_preprocessed = preprocessor.fit_transform(X)

# 划分训练集、测试集
X_train, X_test, y_train, y_test = train_test_split(X_preprocessed, y, test_size=0.2, random_state=42)

# 定义神经网络模型
mlp = MLPClassifier(hidden_layer_sizes=(2500),
                    activation='relu',                   
                    max_iter=1000,
                    random_state=76,
                    tol=1e-6,
                    verbose=True)

# 训练模型
mlp.fit(X_train, y_train)

# 预测测试集
y_pred = mlp.predict(X_test)

# 评估模型
accuracy = accuracy_score(y_test, y_pred)
print(f"Accuracy: {accuracy:.4f}")

# 打印分类报告
print(classification_report(y_test, y_pred))