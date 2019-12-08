;;; ~/.doom.d/org-ews.el -*- lexical-binding: t; -*-

(defcustom org-ews-host nil
  "The URI to the used Ews server."
  :type 'string
  :group 'org-ews)

(defcustom org-ews-ews-path "ews/Exchange.asmx"
  "The path to the EWS API relative to `org-ews-host'."
  :type 'string
  :group 'org-ews)

(defcustom org-ews-user nil
  "The username that is used to login to the Ews server."
  :type 'string
  :group 'org-ews)

(defcustom org-ews-password nil
  "The password that is used to login to the Ews server."
  :type 'string
  :group 'org-ews)

(defcustom org-ews-auth-mechanism
  '(:any :basic :plain :ntlm :negotiate)
  "The authentication mechanism used to login to the Ews server."
  :type (list 'symbol)
  :group 'org-ews)

(defcustom org-ews-max-entries 100
  "The maximum amount of entries fetched in a request."
  :type 'integer
  :group 'org-ews)

(defcustom org-ews-days-past 7
  "The amount of days in the past to fetch entries for."
  :type 'integer
  :group 'org-ews)

(defcustom org-ews-days-future 30
  "The amount of days in the future to fetch entries for."
  :type 'integer
  :group 'org-ews)

(defcustom org-ews-sync-interval nil
  "The amount of seconds until `org-ews-sync' is called. \
If this is nil `org-ews-sync' is never called automatically."
  :type 'integer
  :group 'org-ews)

(defcustom org-ews--timer nil
  "A timer that calls `org-ews-sync' upon expiry."
  :type 'timer
  :group 'org-ews)

(defun org-ews-start ()
  "Call `org-ews-sync' and start a timer that calls `org-ews-sync' every interval specified by `org-ews-sync-interval'."
  (interactive)
  (let ((timer (run-at-time nil org-ews-sync-interval org-ews-sync)))
    (setq org-ews--timer timer)))

(defun org-ews-stop ()
  "Stop the periodic sync."
  (interactive)
  (cancel-timer 'org-ews--timer)
  )

(defcustom org-ews--request-template "<?xml version=\"1.0\" encoding=\"utf-8\"?> \
<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" \
               xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" \
               xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" \
               xmlns:t=\"http://schemas.microsoft.com/ews/services/2006/types\"> \
  <soap:Body> \
    <FindItem Traversal=\"Shallow\" xmlns=\"http://schemas.microsoft.com/ews/services/2006/messages\"> \
      <ItemShape> \
        <t:BaseShape>Default</t:BaseShape> \
        <t:AdditionalProperties> \
          <t:FieldURI FieldURI=\"calendar:MyResponseType\"/> \
        </t:AdditionalProperties> \
      </ItemShape> \
      <CalendarView MaxEntriesReturned=\"%d\" StartDate=\"%sT00:00:00-08:00\" EndDate=\"%sT00:00:00-08:00\"/> \
      <ParentFolderIds> \
        <t:DistinguishedFolderId Id=\"calendar\"/> \
      </ParentFolderIds> \
    </FindItem> \
  </soap:Body> \
</soap:Envelope>" "A template for a SOAP request to an Exchange server."
:type 'string
:group 'org-ews)

(defun org-ews--format-request ()
  "Format a SOAP request with the configured parameters."
  (let* ((get-date-from-delta (lambda (delta)
                                "Return the date `delta' days from today in Org date format."
                                (org-read-date nil nil (format "%+d" delta))))
         (start-date (funcall get-date-from-delta (- org-ews-days-past)))
         (end-date (funcall get-date-from-delta org-ews-days-future)))
    (format org-ews--request-template org-ews-max-entries start-date end-date)))

(defun org-ews-sync ()
  ""
  (interactive)
  (ignore))
