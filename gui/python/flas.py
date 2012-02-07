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


class ModesWidget(QtGui.QWidget):
    u"""Виджет выбора режима"""
    
    
    expanded = QtCore.Signal(str)
    
    def __init__(self, template):
        super(ModesWidget, self).__init__()

        self.setAttribute(QtCore.Qt.WA_DeleteOnClose)
        
        self.tree = QtGui.QTreeWidget()
        self.tree.itemClicked.connect(self.mode_expanded)
        self.tree.setHeaderHidden(True)
        self.tree.setColumnCount(3)
        self.tree.hideColumn(1)
        self.tree.hideColumn(2)
        
        self.grid = QtGui.QGridLayout()
        self.grid.addWidget(self.tree)
        
        self.populate_tree(template, self.tree)
        self.setLayout(self.grid)
    
    def populate_tree(self, submodes, mode):
        if mode is not self.tree:
            mode, = self.tree.findItems(mode, QtCore.Qt.MatchExactly, 1)
            
        for item in submodes:
            type = submodes[item].get('Type')
            text = submodes[item].get('Text')
            #actions = submodes[item].get('Action')
            children = str(submodes[item].get('Children'))
            
            submode = QtGui.QTreeWidgetItem(mode, (text, item, children))

    def mode_expanded(self, item, column=0):
        if item.text(2) == 'True':
            self.expanded.emit(item.text(1))
            item.setExpanded(True)
            item.setText(2, 'False')


class DictionaryWidget(QtGui.QComboBox):
    u"""Виджет со справочником"""

    def __init__(self, text=None):
        super(DictionaryWidget, self).__init__()

        self.setAttribute(QtCore.Qt.WA_DeleteOnClose)
        
        self.setEditable(True)
        self.setEditText(text)


class MdiDocument(QtGui.QWidget):
    u"""Динамическое окно MDI.
    
    На вход получает словарь с шаблоном интерфейса и словарь с данными для
    заполнения. На выход подаёт словарь с данными из форм.
    
    """

    def __init__(self):
        super(MdiDocument, self).__init__()

        self.setAttribute(QtCore.Qt.WA_DeleteOnClose)

    def select_widget(self, type=None, text=None, actions=None):
        if type == 'Dictionary':
            return DictionaryWidget(text)
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
        else:
            return QtGui.QLineEdit(text)
        
    def select_align(self, align=None):
        if align == 'Left':
            return QtCore.Qt.AlignLeft
        elif align == 'Right':
            return QtCore.Qt.AlignRight
        elif align == 'Center':
            return QtCore.Qt.AlignHCenter
        else:
            return QtCore.Qt.AlignLeft
        
    def draw_widgets(self, template):
        self.grid = QtGui.QGridLayout()
        for item in template:
            type = template[item].get('Type')
            text = template[item].get('Text')
            actions = template[item].get('Action')
            widget = self.select_widget(type, text, actions)
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

    def fill_widgets(self, content):
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
        self.draw_widgets(template)
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
        self.draw_widgets(template)
        self.fill_widgets(content)
        self.setWindowTitle("[*]"+name)


class MainWindow(QtGui.QMainWindow):
    u"""Главное окно программы с MDI и тулбарами.
    
    Оперирует окнами, приводит полученные извне данные в подходящий для них
    формат. Забирает из данные из базы.
    
    """
    
    def __init__(self, database):
        super(MainWindow, self).__init__()

        self.setAttribute(QtCore.Qt.WA_DeleteOnClose)
        
        self.mdi = QtGui.QMdiArea()
        self.setCentralWidget(self.mdi)

        self.db = database
        self.document_number = 1
        
        self.create_actions()
        self.create_menus()
        self.create_toolbars()
        self.create_statusbar()
        self.show_modes()
        
        self.setWindowTitle(self.tr("FLAS"))
        self.showMaximized()
        
    def show_modes(self):
        submodes = self.get_submodes()
        mode_selector = ModesWidget(json.loads(submodes))
        self.mdi.addSubWindow(mode_selector)
        mode_selector.expanded.connect(self.expand_mode)
        self.statusBar().showMessage("Modes")
        
    def expand_mode(self, mode):
        submodes = self.get_submodes(mode)
        self.sender().populate_tree(json.loads(submodes), mode)
        
    def get_submodes(self, mode=''):
        query = QtSql.QSqlQuery(self.db)
        query.exec_("SELECT app.modes_node_children_get('{0}')".format(mode))
        query.next()
        return query.value(0)
        
    def dump_json(self, data):
        return json.dumps(data, sort_keys=True, indent=4*' ')
        
    def new(self):
        blank = self.readfile_dialog("ko1blank.json")
        subwindow = self.create_subwindow()
        subwindow.new(json.loads(blank), self.document_number)
        self.document_number += 1
        subwindow.show()
        self.statusBar().showMessage("New")

    def open(self):
        blank = self.readfile_dialog("ko1blank.json")
        document = self.readfile_dialog("ko1document.json")
        subwindow = self.create_subwindow()
        subwindow.open(json.loads(blank),
                       json.loads(document), "ko1document.json")
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
        self.open_act.setStatusTip("Open an existing file")
        self.open_act.triggered.connect(self.open)

        self.save_act = QtGui.QAction("&Save", self)
        self.save_act.setShortcut("Ctrl+S")
        self.save_act.setStatusTip("Save the document to disk")
        self.save_act.triggered.connect(self.save)

        self.exit_act = QtGui.QAction("E&xit", self)
        self.exit_act.setShortcut("Ctrl+Q")
        self.exit_act.setStatusTip("Exit the application")
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


def connect_database(config):
    db = QtSql.QSqlDatabase.addDatabase("QPSQL")
    db.setHostName(config.get('hostname'))
    db.setDatabaseName(config.get('database'))
    db.setUserName(config.get('username'))
    db.setPassword(config.get('password'))
    db.open()
    return db


if __name__ == "__main__":
    translator = QtCore.QTranslator()
    translator.load('translations/ru_RU')
    app = QtGui.QApplication(sys.argv)
    app.installTranslator(translator)
    
    config_file = open('config.json')
    config = json.load(config_file)
    db = connect_database(config)
    mainwindow = MainWindow(db)
    mainwindow.show()
    
    sys.exit(app.exec_())