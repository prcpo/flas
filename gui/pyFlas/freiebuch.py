#!/usr/bin/env python
# coding: utf-8

import sys
import simplejson as json
from PySide import QtCore, QtGui

import mdi_rc


class MdiDocument(QtGui.QWidget):
    """Abstract document subwindow"""

    def __init__(self):
        super(MdiDocument, self).__init__()

        self.setAttribute(QtCore.Qt.WA_DeleteOnClose)

    def select_widget(self, type):
        if type == 'Dictionary':
            return QtGui.QComboBox()
        elif type == 'Label':
            return QtGui.QLabel()
        elif type == 'DocNumber':
            return QtGui.QLabel()
        elif type == 'Date':
            return QtGui.QLabel()
        elif type == 'Sum':
            return QtGui.QLineEdit()
        elif type == 'Text':
            return QtGui.QTextEdit()
        else:
            return QtGui.QLineEdit()
            
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
            widget = self.select_widget(template[item]['Type'])
            widget.setObjectName(item)
            
            if 'Position' in template[item]:
                pos = template[item]['Position']
                
                align = self.select_align(pos['Align'])
                
                if 'RowSpan' in pos and 'ColSpan' in pos:
                    rowspan = pos['RowSpan']
                    colspan = pos['ColSpan']
                else:
                    rowspan = 1
                    colspan = 1
                
                self.grid.addWidget(widget,
                                    pos['Row'], pos['Col'],
                                    rowspan, colspan)#, align)
            else:
                self.grid.addWidget(widget)
        self.showMaximized()
        self.setLayout(self.grid)

    def fill_document(self, content):
        for item in content:
            child = self.findChild(QtGui.QWidget, item)
            if isinstance(child, (QtGui.QLabel, QtGui.QLineEdit)):
                child.setText(content[item])
            elif isinstance(child, QtGui.QComboBox):
                child.addItem(content[item])
            elif isinstance(child, QtGui.QTextEdit):
                child.document().setPlainText(content[item])
                
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
    """Main window with MDI and toolbars"""
    
    def __init__(self, parent=None):
        super(MainWindow, self).__init__()

        self.mdi = QtGui.QMdiArea()
        self.setCentralWidget(self.mdi)

        self.document_number = 1
        self.create_actions()
        self.create_menus()
        self.create_toolbars()
        self.create_statusbar()

        self.setWindowTitle(self.tr("FreieBuch"))
        
        #self.showMaximized()

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
                                self.tr("<b>FreieBuch</b>"))

    def create_subwindow(self):
        subwindow = MdiDocument()
        self.mdi.addSubWindow(subwindow)
        return subwindow

    def create_actions(self):
        self.newAct = QtGui.QAction(QtGui.QIcon(":/images/new.png"),
                                    "&New", self)
        self.newAct.setShortcut("Ctrl+N")
        self.newAct.setStatusTip(self.tr("Create a new file"))
        self.newAct.triggered.connect(self.new)

        self.openAct = QtGui.QAction(QtGui.QIcon(":/images/open.png"),
                                     "&Open...", self)
        self.openAct.setShortcut("Ctrl+O")
        self.openAct.setStatusTip(self.tr("Open an existing file"))
        self.openAct.triggered.connect(self.open)

        self.saveAct = QtGui.QAction(QtGui.QIcon(":/images/save.png"),
                                     "&Save", self)
        self.saveAct.setShortcut("Ctrl+S")
        self.saveAct.setStatusTip(self.tr("Save the document to disk"))
        self.saveAct.triggered.connect(self.save)

        self.exitAct = QtGui.QAction("E&xit", self)
        self.exitAct.setShortcut("Ctrl+Q")
        self.exitAct.setStatusTip(self.tr("Exit the application"))
        self.exitAct.triggered.connect(self.close)

        self.separatorAct = QtGui.QAction(self)
        self.separatorAct.setSeparator(True)

        self.aboutAct = QtGui.QAction("&About", self)
        self.aboutAct.triggered.connect(self.about)

    def create_menus(self):
        self.fileMenu = self.menuBar().addMenu(self.tr("&File"))
        self.fileMenu.addAction(self.newAct)
        self.fileMenu.addAction(self.openAct)
        self.fileMenu.addAction(self.saveAct)
        self.fileMenu.addSeparator()
        self.fileMenu.addAction(self.exitAct)

        self.menuBar().addSeparator()

        self.helpMenu = self.menuBar().addMenu(self.tr("&Help"))
        self.helpMenu.addAction(self.aboutAct)

    def create_toolbars(self):
        self.fileToolBar = self.addToolBar("File")
        self.fileToolBar.addAction(self.newAct)
        self.fileToolBar.addAction(self.openAct)
        self.fileToolBar.addAction(self.saveAct)

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