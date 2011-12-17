#!/usr/bin/env python
# coding: utf-8
###############################################################################
##
## Свободная Бухгалтерия
##
###############################################################################


import sys
from PySide import QtCore, QtGui, QtSql
import simplejson as json


class ModeSelector(QtGui.QWidget):
    """Mode selector widget"""

    def __init__(self, text = None):
        super(ModeSelector, self).__init__()

        self.setAttribute(QtCore.Qt.WA_DeleteOnClose)
        
        self.grid = QtGui.QGridLayout()
        button = QtGui.QPushButton(text)
        self.grid.addWidget(button)
        self.setLayout(self.grid)


class MdiDocument(QtGui.QWidget):
    u"""Динамическое окно MDI.
    
    На вход получает словарь с шаблоном интерфейса и словарь с данными для
    заполнения. На выход подаёт словарь с данными из форм.
    
    """

    def __init__(self):
        super(MdiDocument, self).__init__()

        self.setAttribute(QtCore.Qt.WA_DeleteOnClose)

    def select_widget(self, type, text = None):
        if type == 'Dictionary':
            return QtGui.QComboBox(text)
        elif type == 'Label':
            return QtGui.QLabel(text)
        elif type == 'DocNumber':
            return QtGui.QLabel(text)
        elif type == 'Date':
            return QtGui.QLabel(text)
        elif type == 'Sum':
            return QtGui.QLineEdit(text)
        elif type == 'Text':
            return QtGui.QTextEdit(text)
        elif type == 'Mode':
            return ModeSelector(text)
        else:
            return QtGui.QLineEdit(text)
        
    def select_align(self, align):
        if align == 'Left':
            return QtCore.Qt.AlignLeft
        elif align == 'Right':
            return QtCore.Qt.AlignRight
        elif align == 'Center':
            return QtCore.Qt.AlignHCenter
        else:
            return QtCore.Qt.AlignLeft
        
    def draw_document(self, template):
        self.grid = QtGui.QGridLayout()
        for item in template:
            type = template[item].get('Type')
            text = template[item].get('Text')
            widget = self.select_widget(type, text)
            widget.setObjectName(item)
            pos = template[item].get('Position')
            if pos is not None:
                row = pos.get('Row')
                rowspan = pos.get('RowSpan', 1)
                col = pos.get('Col')
                colspan = pos.get('ColSpan', 1)
                align = self.select_align(pos.get('Align'))
                self.grid.addWidget(widget, row, col, rowspan, colspan)#, align)
            else:
                self.grid.addWidget(widget)
        self.setLayout(self.grid)
        self.parentWidget().adjustSize()

    def fill_document(self, content):
        for item in content:
            child = self.findChild(QtGui.QWidget, item)
            if isinstance(child, (QtGui.QLabel, QtGui.QLineEdit)):
                child.setText(content[item])
            elif isinstance(child, QtGui.QComboBox):
                child.addItem(content[item])
            elif isinstance(child, QtGui.QTextEdit):
                child.document().setPlainText(content[item])
        self.parentWidget().adjustSize()
        
    def new(self, template, number):
        self.draw_document(template)
        self.setWindowTitle("[*]document{0}".format(number))
        
    def save(self):
        data = {}
        for child in self.children():
            if isinstance(child, (QtGui.QLabel, QtGui.QLineEdit)):
                data[child.objectName()] = child.text()
            elif isinstance(child, QtGui.QComboBox):
                data[child.objectName()] = child.currentText()
            elif isinstance(child, QtGui.QTextEdit):
                data[child.objectName()] = child.document().toPlainText()
        return data
    
    def open(self, template, content, name):
        self.draw_document(template)
        self.fill_document(content)
        self.setWindowTitle("[*]"+name)


class MainWindow(QtGui.QMainWindow):
    u"""Главное окно программы с MDI и тулбарами.
    
    Оперирует окнами, приводит полученные извне данные в подходящий для них
    формат. Подключается к базе данных, забирает из неё данные.
    
    """
    
    def __init__(self, parent=None):
        super(MainWindow, self).__init__()

        self.setAttribute(QtCore.Qt.WA_DeleteOnClose)
        
        self.mdi = QtGui.QMdiArea()
        self.setCentralWidget(self.mdi)

        self.document_number = 1
        self.create_actions()
        self.create_menus()
        self.create_toolbars()
        self.create_statusbar()
        self.connect_database()
        self.show_modes()
        
        self.setWindowTitle(self.tr("FLAS"))
        self.showMaximized()
        
    def show_modes(self):
        subwindow = self.create_subwindow()
        modes = self.select_modes()
        print(modes)
        #modes = self.readfile_dialog("modes.json")
        subwindow.new(self.load_json(modes), self.document_number)
        self.document_number += 1
        subwindow.show()
        self.statusBar().showMessage("Modes")
        
    def connect_database(self):
        self.db = QtSql.QSqlDatabase.addDatabase("QPSQL")
        self.db.setHostName("localhost")
        self.db.setDatabaseName("flas")
        self.db.setUserName("postgres")
        self.db.setPassword("postgres")
        self.db.open()
        
    def select_modes(self):
        query = QtSql.QSqlQuery(self.db)
        query.exec_("SELECT app.modes_node_children_get()")
        query.next()
        return query.value(0)
        
    def load_json(self, string):
        return json.loads(string)
        
    def dump_json(self, data):
        return json.dumps(data, sort_keys=True, indent=4 * ' ')
        
    def new(self):
        subwindow = self.create_subwindow()
        blank = self.readfile_dialog("ko1blank.json")
        subwindow.new(self.load_json(blank), self.document_number)
        self.document_number += 1
        subwindow.show()
        self.statusBar().showMessage("New")

    def open(self):
        blank = self.readfile_dialog("ko1blank.json")
        document = self.readfile_dialog("ko1document.json")
        subwindow = self.create_subwindow()
        subwindow.open(self.load_json(blank),
                       self.load_json(document), "ko1document.json")
        subwindow.show()
        self.statusBar().showMessage("Open")
        #subwindow.close()

    def readfile_dialog(self, name):
        filename, filter = QtGui.QFileDialog.getOpenFileName(self, name,
                                                             ".", name)
        file = QtCore.QFile(filename)
        if not file.open(QtCore.QFile.ReadOnly | QtCore.QFile.Text):
            return None
        instream = QtCore.QTextStream(file)
        QtGui.QApplication.setOverrideCursor(QtCore.Qt.WaitCursor)
        string = instream.readAll()
        QtGui.QApplication.restoreOverrideCursor()
        return string
        
    def save(self):
        data = self.mdi.activeSubWindow().widget().save()
        string = self.dump_json(data)
        self.writefile_dialog("ko1document.json", string)
        self.statusBar().showMessage("Save")
        
    def writefile_dialog(self, name, string):
        filename, filter = QtGui.QFileDialog.getSaveFileName(self, name,
                                                             ".", name)
        file = QtCore.QFile(filename)
        if not file.open(QtCore.QFile.WriteOnly | QtCore.QFile.Text):
            return False
        outstream = QtCore.QTextStream(file)
        QtGui.QApplication.setOverrideCursor(QtCore.Qt.WaitCursor)
        outstream << string
        QtGui.QApplication.restoreOverrideCursor()
        return True

    def about(self):
        QtGui.QMessageBox.about(self, self.tr("About"),
                                self.tr("<b>FLAS</b>"))

    def create_subwindow(self):
        subwindow = MdiDocument()
        self.mdi.addSubWindow(subwindow)
        return subwindow

    def create_actions(self):
        self.new_act = QtGui.QAction("&New", self)
        self.new_act.setShortcut("Ctrl+N")
        self.new_act.setStatusTip(self.tr("Create a new file"))
        self.new_act.triggered.connect(self.new)

        self.open_act = QtGui.QAction("&Open...", self)
        self.open_act.setShortcut("Ctrl+O")
        self.open_act.setStatusTip(self.tr("Open an existing file"))
        self.open_act.triggered.connect(self.open)

        self.save_act = QtGui.QAction("&Save", self)
        self.save_act.setShortcut("Ctrl+S")
        self.save_act.setStatusTip(self.tr("Save the document to disk"))
        self.save_act.triggered.connect(self.save)

        self.exit_act = QtGui.QAction("E&xit", self)
        self.exit_act.setShortcut("Ctrl+Q")
        self.exit_act.setStatusTip(self.tr("Exit the application"))
        self.exit_act.triggered.connect(self.close)

        self.separator_act = QtGui.QAction(self)
        self.separator_act.setSeparator(True)

        self.about_act = QtGui.QAction("&About", self)
        self.about_act.triggered.connect(self.about)

    def create_menus(self):
        self.file_menu = self.menuBar().addMenu(self.tr("&File"))
        self.file_menu.addAction(self.new_act)
        self.file_menu.addAction(self.open_act)
        self.file_menu.addAction(self.save_act)
        self.file_menu.addSeparator()
        self.file_menu.addAction(self.exit_act)

        self.menuBar().addSeparator()

        self.help_menu = self.menuBar().addMenu(self.tr("&Help"))
        self.help_menu.addAction(self.about_act)

    def create_toolbars(self):
        self.file_toolbar = self.addToolBar("File")
        self.file_toolbar.addAction(self.new_act)
        self.file_toolbar.addAction(self.open_act)
        self.file_toolbar.addAction(self.save_act)

    def create_statusbar(self):
        self.statusBar().showMessage("Ready")


if __name__ == "__main__":
    translator = QtCore.QTranslator()
    translator.load('translations/ru_RU')
    app = QtGui.QApplication(sys.argv)
    app.installTranslator(translator)
    
    mainwindow = MainWindow()
    mainwindow.show()
    
    sys.exit(app.exec_())