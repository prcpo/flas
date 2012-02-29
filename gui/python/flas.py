#!/usr/bin/env python
# coding: utf-8
###############################################################################
##
## Свободная Бухгалтерия
##
###############################################################################


import sys
from PySide import QtCore, QtGui, QtSql
from PySide.QtCore import QLocale
import simplejson as json
from win32com import client
import zipfile
import os.path


class ModesWidget(QtGui.QWidget):
    """Виджет выбора режима"""
    
    expanded = QtCore.Signal(str)
    
    def __init__(self, template=None):
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
        
        if template is not None:
            self.populate_tree(template, self.tree)
        self.setLayout(self.grid)
        
    def set_template(self, template):
        self.populate_tree(template, self.tree)
    
    def populate_tree(self, submodes, mode):
        if mode is not self.tree:
            mode, = self.tree.findItems(mode, QtCore.Qt.MatchExactly, 1)
            
        for item in submodes:
            #type = submodes[item].get('Type')
            #actions = submodes[item].get('Action')
            text = submodes[item].get('Text')
            children = str(submodes[item].get('Children'))
            QtGui.QTreeWidgetItem(mode, (text, item, children))

    def mode_expanded(self, item):
        if item.text(2) == 'True':
            self.expanded.emit(item.text(1))
            item.setExpanded(True)
            item.setText(2, 'False')


class DictionaryWidget(QtGui.QComboBox):
    """Виджет со спрвочником"""

    def __init__(self, text=None):
        super(DictionaryWidget, self).__init__()

        self.setAttribute(QtCore.Qt.WA_DeleteOnClose)
        
        self.setEditable(True)
        self.setEditText(text)


class MdiDocument(QtGui.QWidget):
    """Динамическое окно MDI.
    
    На вход получает словарь с шаблоном интерфейса и словарь с данными для
    заполнения. На выход подаёт словарь с данными из форм.
    
    """

    def __init__(self):
        super(MdiDocument, self).__init__()

        self.setAttribute(QtCore.Qt.WA_DeleteOnClose)

    def select_widget(self, type=None, text=None):
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
            #actions = template[item].get('Action')
            widget = self.select_widget(type, text)
            widget.setObjectName(item)
            pos = template[item].get('Position')
            if pos is not None:
                row = pos.get('Row')
                rowspan = pos.get('RowSpan', 1)
                col = pos.get('Col')
                colspan = pos.get('ColSpan', 1)
                #align = self.select_align(pos.get('Align'))
                self.grid.addWidget(widget, row, col, rowspan, colspan)
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
        self.setWindowTitle('[*]document{0}'.format(number))
        
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
        current_date = QtCore.QDate.currentDate()
        content['numeric_creation_date'] = current_date.toString('dd.MM.yyyy')
        content['alphabetic_creation_date_day'] = current_date.toString('dd')
        content['alphabetic_creation_date_month'] = current_date.toString('MMMM')
        content['alphabetic_creation_date_year'] = current_date.toString('yyyy')
        self.fill_widgets(content)
        self.setWindowTitle('[*]'+name)

        
class MainWindow(QtGui.QMainWindow):
    """Главное окно программы с MDI и тулбарами.
    
    Оперирует окнами, приводит полученные извне данные в подходящий для них
    формат. Забирает из данные из базы.
    
    """
    
    def __init__(self):
        super(MainWindow, self).__init__()

        self.setAttribute(QtCore.Qt.WA_DeleteOnClose)
        
        self.mdi = QtGui.QMdiArea()
        self.setCentralWidget(self.mdi)
        
        self.configure()
        self.connect_database(self.config)
        
        self.create_actions()
        #self.create_menus()
        self.create_toolbars()
        self.create_statusbar()
        #self.show_modes()
        self.new()
        
        self.setWindowTitle(self.tr('FLAS'))
        self.showMaximized()
    
    def configure(self):
        config_file = open('config.json')
        self.config = json.load(config_file)
        self.printer = self.config.get('printer')
        self.document_number = 1

    def connect_database(self, config):
        self.db = QtSql.QSqlDatabase.addDatabase('QPSQL')
        self.db.setHostName(config.get('hostname'))
        self.db.setDatabaseName(config.get('database'))
        self.db.setUserName(config.get('username'))
        self.db.setPassword(config.get('password'))
        self.db.open()
    
    def create_subwindow(self, subwindow_type):
        if subwindow_type == 'MdiDocument':
            subwindow = MdiDocument()
        elif subwindow_type == 'ModesWidget':
            subwindow = ModesWidget()
        else:
            subwindow = MdiDocument()
        self.mdi.addSubWindow(subwindow)
        return subwindow
        
    def show_modes(self):
        submodes = self.get_submodes()
        submodes_json = json.loads(submodes)
        mode_selector = self.create_subwindow('ModesWidget')
        mode_selector.set_template(submodes_json)
        mode_selector.expanded.connect(self.expand_mode)
        self.statusBar().showMessage('Modes')
        
    def expand_mode(self, mode):
        submodes = self.get_submodes(mode)
        self.sender().populate_tree(json.loads(submodes), mode)
        
    def get_submodes(self, mode=''):
        query = QtSql.QSqlQuery(self.db)
        query.exec_("SELECT app.modes_node_children_get('{0}')".format(mode))
        query.next()
        return query.value(0)
        
    def new(self):
        blank_file = QtCore.QFile('documents/ko1blank.json')
        blank_file.open(QtCore.QFile.ReadOnly | QtCore.QFile.Text)
        instream = QtCore.QTextStream(blank_file)
        instream.setCodec("UTF-8")
        blank_json = instream.readAll()
        blank = json.loads(blank_json)
        
        document_file = QtCore.QFile('documents/ko1document.json')
        document_file.open(QtCore.QFile.ReadOnly | QtCore.QFile.Text)
        instream = QtCore.QTextStream(document_file)
        instream.setCodec("UTF-8")
        document_json = instream.readAll()
        document = json.loads(document_json)

        #for item in document:
        #    print('Text type: '+repr(type(document[item])))
        #    print('Text: '+document[item])
        
        #blank = self.readfile_dialog()
        subwindow = self.create_subwindow('MdiDocument')
        subwindow.open(blank, document, 'Document')
        
        #self.document_number += 1
        subwindow.show()
        self.statusBar().showMessage('New')

    def open(self):
        blank = self.readfile_dialog()
        document = self.readfile_dialog()
        subwindow = self.create_subwindow('MdiDocument')
        subwindow.open(json.loads(blank),
                       json.loads(document), 'ko1document.json')
        subwindow.show()
        self.statusBar().showMessage('Open')

    def readfile_dialog(self):
        filename, filter = QtGui.QFileDialog.getOpenFileName(self, dir='.')
        file = QtCore.QFile(filename)
        if not file.open(QtCore.QFile.ReadOnly | QtCore.QFile.Text):
            return None
        instream = QtCore.QTextStream(file)
        instream.setCodec("UTF-8")
        QtGui.QApplication.setOverrideCursor(QtCore.Qt.WaitCursor)
        data = instream.readAll()
        QtGui.QApplication.restoreOverrideCursor()
        return data
        
    def save(self):
        content = self.mdi.activeSubWindow().widget().save()
        data = json.dumps(content, sort_keys=True, indent=4*' ')
        self.writefile_dialog(data)
        self.statusBar().showMessage('Save')
        
    def writefile_dialog(self, data):
        filename, filter = QtGui.QFileDialog.getSaveFileName(self, dir='.')
        file = QtCore.QFile(filename)
        if not file.open(QtCore.QFile.WriteOnly | QtCore.QFile.Text):
            return False
        outstream = QtCore.QTextStream(file)
        instream.setCodec("UTF-8")
        QtGui.QApplication.setOverrideCursor(QtCore.Qt.WaitCursor)
        outstream << data
        QtGui.QApplication.restoreOverrideCursor()
        return True

    def printfile(self):
        if self.printer is None:
            self.printer_dialog()
        
        content = self.mdi.activeSubWindow().widget().save()
        self.fill_blank(content)

        filename = os.path.abspath('documents/ko1document.odt')

        word = client.Dispatch('Word.Application')
        if filename is not None:
            word.Documents.Open(filename)
            word.ActivePrinter = self.printer
            word.ActiveDocument.PrintOut()
        
        if not word.BackgroundPrintingStatus:
            word.ActiveDocument.Close()
            word.Quit()
        
    def printer_dialog(self):
        printer = QtGui.QPrinter()
        dialog = QtGui.QPrintDialog(printer, self)
        dialog.accepted.connect(self.set_printer)
        dialog.exec_()
        
    def set_printer(self, printer):
        self.printer = printer.printerName()
        
    def fill_blank(self, document):
        compression = zipfile.ZIP_DEFLATED
        odf = {}
        
        with zipfile.ZipFile('documents/ko1blank.odt') as file:
            namelist = file.namelist()
            for name in namelist:
                odf[name] = file.read(name).decode('utf-8')

        document_sorted = sorted(document.keys(), key=len, reverse=True)

        for item in document_sorted:
            odf['content.xml'] = odf['content.xml'].replace(item, document[item])

        with zipfile.ZipFile('documents/ko1document.odt', 'w') as file:
            for name in namelist:
                file.writestr(name, odf[name].encode('utf-8'), compression)
        
    def about(self):
        QtGui.QMessageBox.about(self, self.tr('About'),
                                self.tr('<b>FLAS</b>'))

    def create_actions(self):
        self.new_act = QtGui.QAction('&New', self)
        self.new_act.setShortcut('Ctrl+N')
        self.new_act.setStatusTip(self.tr('Create a new file'))
        self.new_act.triggered.connect(self.new)

        self.open_act = QtGui.QAction('&Open...', self)
        self.open_act.setShortcut('Ctrl+O')
        self.open_act.setStatusTip('Open an existing file')
        self.open_act.triggered.connect(self.open)

        self.save_act = QtGui.QAction(self.tr('&Save'), self)
        self.save_act.setShortcut('Ctrl+S')
        self.save_act.setStatusTip(self.tr('Save the document to disk'))
        self.save_act.triggered.connect(self.save)
        
        self.printfile_act = QtGui.QAction(self.tr('&Print'), self)
        self.printfile_act.setShortcut('Ctrl+P')
        self.printfile_act.triggered.connect(self.printfile)

        self.exit_act = QtGui.QAction(self.tr('E&xit'), self)
        self.exit_act.setShortcut('Ctrl+Q')
        self.exit_act.setStatusTip(self.tr('Exit the application'))
        self.exit_act.triggered.connect(self.close)

        self.separator_act = QtGui.QAction(self)
        self.separator_act.setSeparator(True)

        self.about_act = QtGui.QAction('&About', self)
        self.about_act.triggered.connect(self.about)

    def create_menus(self):
        self.file_menu = self.menuBar().addMenu(self.tr('&File'))
        self.file_menu.addAction(self.new_act)
        self.file_menu.addAction(self.open_act)
        self.file_menu.addAction(self.save_act)
        self.file_menu.addSeparator()
        self.file_menu.addAction(self.exit_act)

        self.menuBar().addSeparator()

        self.help_menu = self.menuBar().addMenu(self.tr('&Help'))
        self.help_menu.addAction(self.about_act)

    def create_toolbars(self):
        self.file_toolbar = self.addToolBar('File')
        self.file_toolbar.addAction(self.new_act)
        #self.file_toolbar.addAction(self.open_act)
        #self.file_toolbar.addAction(self.save_act)
        #self.file_toolbar.addAction(self.open_html_act)
        self.file_toolbar.addAction(self.printfile_act)

    def create_statusbar(self):
        self.statusBar().showMessage('Ready')


if __name__ == '__main__':
    #QtCore.QLocale.setDefault(QtCore.QLocale(QtCore.QLocale.Russian, QtCore.QLocale.RussianFederation))

    #translator = QtCore.QTranslator()
    #translator.load('translations/ru_RU')

    app = QtGui.QApplication(sys.argv)

    #if translator.load('qt_ru', QtCore.QLibraryInfo.location(QtCore.QLibraryInfo.TranslationsPath)):
    #app.installTranslator(translator)

    mainwindow = MainWindow()
    mainwindow.show()
    
    sys.exit(app.exec_())