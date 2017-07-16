from sklearn.metrics import accuracy_score, f1_score, precision_score, \
                            recall_score, roc_auc_score
from sklearn import linear_model

n_train = X_train.shape[0]
X_train_flat = X_train.reshape((n_train, T * 16))

n_test = X_test.shape[0]
X_test_flat = X_test.reshape((n_test, T * 16))

# C: smaller values specify stronger regularization
clf = linear_model.LogisticRegression(C = 16, n_jobs = 1, solver = 'liblinear',
                                      verbose = 5)

clf.fit(X_train_flat, y_train)
clf.score(X_test_flat, y_test)

y_pred_hard = clf.predict(X_test_flat)
y_pred_soft = clf.predict_proba(X_test_flat)[:, 1]

roc = roc_auc_score(y_test, y_pred_soft)

accuracy_score(y_test, y_pred_hard)
precision_score(y_test, y_pred_hard)
recall_score(y_test, y_pred_hard)
f1_score(y_test, y_pred_hard, average='binary')

