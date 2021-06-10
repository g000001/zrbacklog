;;; -*- mode: Lisp; coding: utf-8  -*-

(cl:in-package "CL-USER")


(ql:quickload '(bcl alexandria st-json drakma kebab zreclos lambda.output))


(defpackage zrbacklog
  (:use bcl alexandria st-json zreclos srfi-2)
  (:shadowing-import-from zreclos defclass)
  (:shadow keyword))


(in-package backlog)


(in-syntax *bcl*)


(defvar *init-file-name* (merge-pathnames ".backlog" (user-homedir-pathname)))


(defvar *current-project*)


(defgeneric blpatch (class &key))


(defgeneric blpost (class &key))


(defgeneric backlog-object->jso (obj))


(defgeneric update-backlog-object-from-jso (obj jso))


(defgeneric make-backlog-object-from-jso (class jso))


(defun read-init ()
  (w/infile (in *init-file-name*)
    (read in)))


(defun api-key ()
  (cdr (read-init)))


(defun backlog-space (&optional (path ""))
  (fstring "~A/api/v2/~A" 
           (car (read-init))
           path))


(defun api-path (&rest paths)
  (backlog-space (fstring "~{~A~^/~}?apiKey=~A"
                          paths
                          (api-key))))


(defun camel-plulal (number singular)
  (kebab:to-camel-case (lambda.output:plural number singular)))


(defclass backlog-class (instance-recording-class required-slot-class)
  ((id-slot :initform 'id :accessor id-slot :initarg :id-slot)))


(defclass backlog-object (instance-recording-object)
  ()
  (:metaclass backlog-class))


(Zdefun all (class)
  (scan-direct-instances (find-class class)))


(defun find-instance (item class-name &key (key #'identity))
  (collect-first 
   (choose-if (^ (x)
                (and (typep x class-name)
                     (equal item (funcall key x))))
              (all class-name))))


(defun find-instance-by-name (item class-name)
  (find-instance item class-name :key #'name))


(defmethod ensure-class-using-class :after ((class backlog-class) name &rest initargs)
  (declare (ignore initargs))
  (setf (fdefinition (intern (fstring "$~A" (string-upcase (class-name class)))))
        (^ (&optional name)
          (if name
              (find-instance-by-name name (class-name class))
              (all (class-name class))))))


#+LispWorks
(defmethod clos:process-a-class-option ((class backlog-class)
                                        (name (eql :id-slot))
                                        value)
  (unless (and value (null (cdr value)))
    (error "backlog-class :id-slot must have a single value."))
  (list name `',(car value)))


(defclass project (backlog-object)
  ((id :accessor id :initarg :id)
   (project-key :accessor project-key :initarg :project-key)
   (name :accessor name :initarg :name)
   (chart-enabled :accessor chart-enabled :initarg :chart-enabled)
   (subtasking-enabled :accessor subtasking-enabled :initarg :subtasking-enabled)
   (project-leader-can-edit-project-leader :accessor project-leader-can-edit-project-leader
                                           :initarg :project-leader-can-edit-project-leader)
   (use-wiki-tree-view :accessor use-wiki-tree-view :initarg :use-wiki-tree-view)
   (text-formatting-rule :accessor text-formatting-rule :initarg :text-formatting-rule)
   (archived :accessor archived :initarg :archived)
   (display-order :accessor display-order :initarg :display-order)
   (use-dev-attributes :accessor use-dev-attributes :initarg :use-dev-attributes))
  (:metaclass backlog-class))


(defclass user (backlog-object)
  ((id :accessor id :initarg :id)
   (user-id :accessor user-id :initarg :user-id)
   (name :accessor name :initarg :name)
   (role-type :accessor role-type :initarg :role-type)
   (lang :accessor lang :initarg :lang)
   (mail-address :accessor mail-address :initarg :mail-address)
   (nulab-account :accessor nulab-account :initarg :nulab-account)
   (keyword :accessor keyword :initarg :keyword))
  (:metaclass backlog-class)
  (:id-slot id))


(setf (find-class 'assignee)
      (setf (find-class 'created-user)
            (setf (find-class 'updated-user)
                  (find-class 'user))))


(defclass nulab-account (backlog-object)
  ((nulab-id :accessor nulab-id :initarg :nulab-id)
   (name :accessor name :initarg :name)
   (unique-id :accessor unique-id :initarg :unique-id))
  (:metaclass backlog-class)
  (:id-slot nulab-id))


(defclass priority (backlog-object)
  ((id :accessor id :initarg :id)
   (name :accessor name :initarg :name))
  (:metaclass backlog-class)
  (:id-slot id))


(defclass status (backlog-object)
  ((id :accessor id :initarg :id)
   (project-id :accessor project-id :initarg :project-id)
   (name :accessor name :initarg :name)
   (color :accessor color :initarg :color)
   (display-order :accessor display-order :initarg :display-order))
  (:metaclass backlog-class)
  (:id-slot id))


(defclass issue-type (backlog-object)
  ((id :accessor id :initarg :id)
   (project-id :accessor project-id :initarg :project-id)
   (name :accessor name :initarg :name)
   (color :accessor color :initarg :color)
   (display-order :accessor display-order :initarg :display-order)
   (template-summary :accessor template-summary :initarg :template-summary)
   (template-description :accessor template-description :initarg :template-description))
  (:metaclass backlog-class)
  (:id-slot id))


(defclass issue (backlog-object)
  #||
projectId (必須) 	数値 	課題を登録するプロジェクトのID
summary (必須) 	文字列 	課題の件名
parentIssueId 	数値 	課題の親課題のID
description 	文字列 	課題の詳細
startDate 	文字列 	課題の開始日 (yyyy-MM-dd)
dueDate 	文字列 	課題の期限日 (yyyy-MM-dd)
estimatedHours 	数値 	課題の予定時間
actualHours 	数値 	課題の実績時間
issueTypeId (必須) 	数値 	課題の種別のID
categoryId[] 	数値 	課題のカテゴリーのID
versionId[] 	数値 	課題の発生バージョンのID
milestoneId[] 	数値 	課題のマイルストーンのID
priorityId (必須) 	数値 	課題の優先度のID
assigneeId 	数値 	課題の担当者のID
notifiedUserId[] 	数値 	課題の登録の通知を受け取るユーザーのID
attachmentId[] 	数値 	添付ファイルの送信APIが返すID
カスタム属性
||#
  ((id :accessor id :initarg :id)
   (project-id :accessor project-id :initarg :project-id)
   (issue-key :accessor issue-key :initarg :issue-key)
   (key-id :accessor key-id :initarg :key-id)
   (issue-type :accessor issue-type :initarg :issue-type)
   (summary :accessor summary :initarg :summary)
   (description :accessor description :initarg :description)
   (resolution :accessor resolution :initarg :resolution)
   (priority :accessor priority :initarg :priority :initform ($priority "中"))
   (status :accessor status :initarg :status)
   (assignee :accessor assignee :initarg :assignee)
   (category :accessor category :initarg :category :initform '())
   (versions :accessor versions :initarg :versions :initform '())
   (milestone :accessor milestone :initarg :milestone)
   (start-date :accessor start-date :initarg :start-date :initform :null)
   (due-date :accessor due-date :initarg :due-date :initform :null)
   (estimated-hours :accessor estimated-hours :initarg :estimated-hours)
   (actual-hours :accessor actual-hours :initarg :actual-hours)
   (parent-issue-id :accessor parent-issue-id :initarg :parent-issue-id :initform :null)
   (created-user :accessor created-user :initarg :created-user)
   (created :accessor created :initarg :created)
   (updated-user :accessor updated-user :initarg :updated-user)
   (updated :accessor updated :initarg :updated)
   (custom-fields :accessor custom-fields :initarg :custom-fields)
   (attachments :accessor attachments :initarg :attachments)
   (shared-files :accessor shared-files :initarg :shared-files)
   (stars :accessor stars :initarg :stars))
  (:metaclass backlog-class)
  (:id-slot id)
  (:required-slots project-id summary issue-type priority))


(defclass comment (backlog-object) 
  ((id :accessor id :initarg :id)
   (content :accessor content :initarg :content)
   (change-log :accessor change-log :initarg :change-log)
   (created-user :accessor created-user :initarg :created-user)
   (created :accessor created :initarg :created)
   (updated :accessor updated :initarg :updated)
   (stars :accessor stars :initarg :stars)
   (notifications :accessor notifications :initarg :notifications))
  (:metaclass backlog-class)
  (:id-slot id)
  (:required-slots content))


(defun make-null-initargs (class)
  (if (subtypep (class-of class) 'required-slot-class)
      (mapcan (^ (s)
                (list (intern (string s) :keyword) :null))
              (class-required-slots class))
      '()))


(defmethod make-backlog-object-from-jso ((class backlog-class) (jso jso))
  (collect-nth 0
               (choose-if (^ (i)
                            (and i (equal (getjso (kebab:to-camel-case (string (id-slot class))) jso)
                                          (get i (id-slot class)))))
                          (scan-direct-instances class))
               ;;default
               (let ((blo (apply #'a class (make-null-initargs class))))
                 (mapjso (^ (k v)
                           (let ((name (intern (string-upcase (kebab:to-kebab-case k)))))
                             (let ((class (find-class name nil)))
                               (if class
                                   (setf (get blo name) 
                                         (make-backlog-object-from-jso class v))
                                   (setf (get blo name) v)))))
                         jso)
                 blo)))


(defmethod make-backlog-object-from-jso ((class backlog-class) (jso (eql :null)))
  :null)


(defmethod update-backlog-object-from-jso ((obj backlog-object) (jso jso))
  (mapjso (^ (k v)
            (let ((name (intern (string-upcase (kebab:to-kebab-case k)))))
              (let ((class (find-class name nil)))
                (if class
                    (setf (get obj name) 
                          (update-backlog-object-from-jso (make-backlog-object-from-jso class v)
                                                          v))
                    (setf (get obj name) v)))))
          jso)
  obj)


(defmethod backlog-object->jso ((obj backlog-object))
  (st-json::make-jso 
   :alist (mapcar (^ (s)
                    (let ((name (slot-definition-name s)))
                      (cons (kebab:to-camel-case (string name))
                            (typecase (get obj name)
                              (backlog-object (backlog-object->jso (get obj name)))
                              (T (get obj name))))))
                  (class-slots (class-of obj)))))


(defun jsget (url)
  (read-json-from-string
   (drakma:http-request url :method :get)))


(defun jspost (url &key content)
  (read-json-from-string
   (drakma:http-request url
                        :method :post
                        :content-type "application/json"
                        :content content)))


(defun jspatch (url &key content)
  (read-json-from-string
   (drakma:http-request url
                        :method :patch
                        :content-type "application/json"
                        :content content)))


(defun blget (class id-or-key)
  (jsget (api-path (camel-plulal 2 class) id-or-key)))


(defun projects (&optional name)
  (let ((all (jsget (api-path "projects"))))
    (if name
        (find-if (^ (x) (equal name (getjso "projectKey" x))) all)
        all)))


(defun %blget* (class project-name)
  (let ((prjid (or (eq 'project class)
                   (id ($project project-name))))
        (classes (camel-plulal 2 class)))
    (case class
      ((project)
       (jsget (api-path "projects")))
      ((status issue-type)
       (jsget (api-path "projects" prjid classes)))
      ((comment)
       (jsget (api-path "issues" "comment" classes)))
      (otherwise 
       (jsget (api-path classes))))))


(defun %download (class-name project)
  (reset-instance-record (find-class class-name))
  (dolist (x (%blget* class-name (name project)))
    (make-backlog-object-from-jso (find-class class-name) x))
  (all class-name))


(defun download (class-name)
  (%download class-name *current-project*))


(defmethod blpatch (class &key content)
  (jspatch (fstring "~(~A~A~:*~P~)?apiKey=~A"
                    (backlog-space)
                    class
                    (api-key))
           :content content))


(defmethod blpatch ((issue issue) &key content (comment "update"))
  (declare (ignore content))
  (jspatch (fstring "~(~A~A~:*~P~)/~A?apiKey=~A"
                    (backlog-space)
                    (class-name (class-of issue))
                    (issue-key issue)
                    (api-key))
           :content (write-json-to-string
                     (jso "summary" (summary issue)
                          "parentIssueId" (parent-issue-id issue)
                          "description" (description issue)
                          "statusId" (id (status issue))
                          "resolutionId" (or (ignore-errors (id (resolution issue)))
                                             :null)
                          "startDate" (start-date issue)
                          "dueDate" (due-date issue)
                          "estimatedHours" (estimated-hours issue)
                          "actualHours" (actual-hours issue)
                          "issueTypeId" (id (issue-type issue))
                          "categoryId" (category issue)
                          #||
versionId[] 	数値 	課題の発生バージョンのID
milestoneId[] 	数値 	課題のマイルストーンのID
priorityId 	数値 	課題の優先度のID
assigneeId 	数値 	課題の担当者のID
notifiedUserId[] 	数値 	課題の登録の通知を受け取るユーザーのID
attachmentId[] 	数値 	添付ファイルの送信APIが返すID
||#
                          "comment" comment))))

(defun setup/project ()
  (let ((project (find-class 'project)))
    (reset-instance-record project)
    (dolist (p (jsget (api-path "projects")))
      (make-backlog-object-from-jso project p))
    (all 'project)))


(defun setup ()
  (setup/project)
  (iterate ((p (all 'project)))
    (let ((*current-project* p))
      (mapc #'download '(issue user priority status issue-type)))))


(defmacro or-null (val)
  `(or (ignore-errors ,val) :null))


(defmethod blpost ((issue issue) &key content)
  (declare (ignore content))
  (jspost (fstring "~(~A~A~:*~P~)?apiKey=~A"
                   (backlog-space)
                   (class-name (class-of issue))
                   (api-key))
          :content (write-json-to-string
                    (jso "projectId" (project-id issue)
                         "summary" (summary issue)
                         "parentIssueId" (or-null (parent-issue-id issue))
                         "description" (or-null (description issue))
                         "startDate" (or-null (start-date issue))
                         "dueDate" (or-null (due-date issue))
                         "estimatedHours" (or-null (estimated-hours issue))
                         "actualHours" (or-null (actual-hours issue))
                         "issueTypeId" (id (issue-type issue))
                         "categoryId" (or-null (mapcar #'id (category issue)))
                         "versionId" (or-null (mapcar #'id (versions issue)))
                         "milestoneId" (or-null (mapcar #'id (milestone issue)))
                         "priorityId" (id (priority issue))
                         "assigneeId" (or-null (id (assignee issue)))
                         ;;"notifiedUserId" (or-null )
                         "attachmentId" (or-null (id (attachments issue)))))))


(defmethod blpost ((comment comment) &key issue)
  (jspost (api-path "issues" (id issue) "comments")
          :content (write-json-to-string (jso "content" (content comment)))))


(defun close-issue (issue &key comment)
  (setf (status issue) ($status "完了"))
  (blpatch issue :comment comment))


(defun new-comment-issue (issue &key comment)
  (blpost (a 'comment :content comment)
          :issue issue))


;;; *EOF*
