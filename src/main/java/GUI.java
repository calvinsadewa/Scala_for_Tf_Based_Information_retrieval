import scala.Tuple2;
import scala.collection.Seq;
import scala.collection.immutable.Set;

import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Created by calvin-pc on 9/24/2015.
 */
public class GUI {
    private JPanel cardPanel;
    private JPanel createIndexPanel;
    private JTextField docCollectionLocation;
    private JTextField stopWordLocation;
    private JComboBox TFComboBox;
    private JCheckBox IDFCheckBox;
    private JCheckBox normalizationCheckBox;
    private JCheckBox porterStemCheckBox;
    private JButton indexButton;
    private JPanel queryPanel;
    private JRadioButton experimentRadioButton;
    private JRadioButton freeSearchRadioButton;
    private JPanel queryModePanel;
    private JPanel experimentModePanel;
    private JPanel freeSearchPanel;
    private JButton backButton;
    private JTextField queryLocationField;
    private JTextField relevanceJudgmentLocation;
    private JButton experimentButton;
    private JTextArea resultTextArea;
    private JComboBox QTFComboBox;
    private JCheckBox QStemCheckBox;
    private JCheckBox QnormalizationCheckBox;
    private JCheckBox QIDFCheckBox;
    private JTextField searchField;
    private JButton searchButton;
    private JComboBox feedbackSelection1;
    private JComboBox feedbackSelection2;
    private JTextField topNTextField;
    private JCheckBox seachSeen;
    private JButton feedbackButton;
    private JTable searchTable;
    private JCheckBox expansionCheckBox;
    Object[] columnNames = {"No", "Title", "Similarity", "Relevant?"};
    Object[][] data = {
            {"1", "dummy", new Double(0), false},
    };
    private coba searchEngine = new coba();
    private coba.TF tfKind = new coba.noTF();
    private coba.FeedbackType feedKind = new coba.roccio();
    private Seq<Tuple2<String,Object>> prev_result = null;

    public GUI() {
        indexButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                switch (TFComboBox.getSelectedIndex() ){
                    case 0 : tfKind = new coba.noTF(); System.out.println("No TF");break;
                    case 1 : tfKind = new coba.rawTF(); System.out.println("Raw TF");break;
                    case 2 : tfKind = new coba.logisticTF(); System.out.println("Log TF");break;
                    case 3 : tfKind = new coba.binaryTF(); System.out.println("Binary TF");break;
                    case 4 : tfKind = new coba.augmentedTF(); System.out.println("Augmented TF");break;
                }

                searchEngine.create_index(
                        tfKind,IDFCheckBox.isSelected()
                        ,normalizationCheckBox.isSelected()
                        ,porterStemCheckBox.isSelected()
                        ,stopWordLocation.getText()
                        ,docCollectionLocation.getText());

                ((CardLayout)cardPanel.getLayout()).show(cardPanel,"query");
            }
        });
        experimentRadioButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                ((CardLayout)queryModePanel.getLayout()).show(queryModePanel,"experiment");
            }
        });
        freeSearchRadioButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                ((CardLayout)queryModePanel.getLayout()).show(queryModePanel,"free_search");
            }
        });
        backButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                ((CardLayout)cardPanel.getLayout()).show(cardPanel,"create_index");
            }
        });
        experimentButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                switch (QTFComboBox.getSelectedIndex() ){
                    case 0 : tfKind = new coba.noTF(); System.out.println("No TF");break;
                    case 1 : tfKind = new coba.rawTF(); System.out.println("Raw TF");break;
                    case 2 : tfKind = new coba.logisticTF(); System.out.println("Log TF");break;
                    case 3 : tfKind = new coba.binaryTF(); System.out.println("Binary TF");break;
                    case 4 : tfKind = new coba.augmentedTF(); System.out.println("Augmented TF");break;
                }
                switch (feedbackSelection2.getSelectedIndex() ){
                    case 0 : feedKind = new coba.roccio(); System.out.println("Roccio");break;
                    case 1 : feedKind = new coba.ide_regular(); System.out.println("Ide Regular");break;
                    case 2 : feedKind = new coba.ide_dec_hi(); System.out.println("Ide Dec Hi");break;
                }

                //No Relevance Feedback
                if (feedbackSelection1.getSelectedIndex() == 0) {
                    resultTextArea.setText(
                            searchEngine.experiments2Text(
                                    searchEngine.experiment(
                                            tfKind,
                                            QIDFCheckBox.isSelected(),
                                            QnormalizationCheckBox.isSelected(),
                                            QStemCheckBox.isSelected(),
                                            queryLocationField.getText(),
                                            relevanceJudgmentLocation.getText())));
                }
                //Relevance Feedback
                else if (feedbackSelection1.getSelectedIndex() == 1) {
                    resultTextArea.setText(
                            searchEngine.experiments2Text(
                                    expansionCheckBox.isSelected() ?
                                            searchEngine.experimentRelevanceFeedbackWithExpansion(
                                                    tfKind,
                                                    QIDFCheckBox.isSelected(),
                                                    QnormalizationCheckBox.isSelected(),
                                                    QStemCheckBox.isSelected(),
                                                    queryLocationField.getText(),
                                                    relevanceJudgmentLocation.getText(),
                                                    Integer.parseInt(topNTextField.getText()),
                                                    feedKind,
                                                    seachSeen.isSelected()) :
                                            searchEngine.experimentRelevanceFeedback(
                                                    tfKind,
                                                    QIDFCheckBox.isSelected(),
                                                    QnormalizationCheckBox.isSelected(),
                                                    QStemCheckBox.isSelected(),
                                                    queryLocationField.getText(),
                                                    relevanceJudgmentLocation.getText(),
                                                    Integer.parseInt(topNTextField.getText()),
                                                    feedKind,
                                                    seachSeen.isSelected())));
                }
                //Pseudo Relevance Feedback
                else if (feedbackSelection1.getSelectedIndex() == 2) {
                    resultTextArea.setText(
                            searchEngine.experiments2Text(
                                    expansionCheckBox.isSelected() ?
                                            searchEngine.experimentPseudoFeedbackWithExpansion(
                                                    tfKind,
                                                    QIDFCheckBox.isSelected(),
                                                    QnormalizationCheckBox.isSelected(),
                                                    QStemCheckBox.isSelected(),
                                                    queryLocationField.getText(),
                                                    relevanceJudgmentLocation.getText(),
                                                    Integer.parseInt(topNTextField.getText()),
                                                    feedKind) :
                                            searchEngine.experimentPseudoFeedback(
                                                    tfKind,
                                                    QIDFCheckBox.isSelected(),
                                                    QnormalizationCheckBox.isSelected(),
                                                    QStemCheckBox.isSelected(),
                                                    queryLocationField.getText(),
                                                    relevanceJudgmentLocation.getText(),
                                                    Integer.parseInt(topNTextField.getText()),
                                                    feedKind)));
                }
            }
        });
        searchButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent actionEvent) {
                switch (QTFComboBox.getSelectedIndex() ){
                    case 0 : tfKind = new coba.noTF(); System.out.println("No TF");break;
                    case 1 : tfKind = new coba.rawTF(); System.out.println("Raw TF");break;
                    case 2 : tfKind = new coba.logisticTF(); System.out.println("Log TF");break;
                    case 3 : tfKind = new coba.binaryTF(); System.out.println("Binary TF");break;
                    case 4 : tfKind = new coba.augmentedTF(); System.out.println("Augmented TF");break;
                }
                switch (feedbackSelection2.getSelectedIndex() ){
                    case 0 : feedKind = new coba.roccio(); System.out.println("Roccio");break;
                    case 1 : feedKind = new coba.ide_regular(); System.out.println("Ide Regular");break;
                    case 2 : feedKind = new coba.ide_dec_hi(); System.out.println("Ide Dec Hi");break;
                }

                //No Relevance Feedback
                if (feedbackSelection1.getSelectedIndex() == 0 || feedbackSelection1.getSelectedIndex() == 1) {
                    Seq<Tuple2<String,Object>> result = searchEngine.search(tfKind,
                            QIDFCheckBox.isSelected(),
                            QnormalizationCheckBox.isSelected(),
                            QStemCheckBox.isSelected(),
                            searchField.getText());

                    prev_result = result;

                    data = new Object[result.length()][];
                    for (int i = 0; i < result.length(); i++) {
                        Tuple2<String,Object> res = result.apply(i);
                        Object[] row = {new Integer(i+1).toString(),res._1(),res._2(),false};
                        data[i] = row;
                    }
                }
                //Pseudo Relevance Feedback
                else if (feedbackSelection1.getSelectedIndex() == 2) {
                    Seq<Tuple2<String,Object>> result = expansionCheckBox.isSelected() ?
                            searchEngine.pseudoFeedbackSearchWithExpansion(tfKind,
                                    QIDFCheckBox.isSelected(),
                                    QnormalizationCheckBox.isSelected(),
                                    QStemCheckBox.isSelected(),
                                    searchField.getText(),
                                    Integer.parseInt(topNTextField.getText()),
                                    feedKind) :
                            searchEngine.pseudoFeedbackSearch(tfKind,
                                    QIDFCheckBox.isSelected(),
                                    QnormalizationCheckBox.isSelected(),
                                    QStemCheckBox.isSelected(),
                                    searchField.getText(),
                                    Integer.parseInt(topNTextField.getText()),
                                    feedKind);

                    prev_result = result;

                    data = new Object[result.length()][];
                    for (int i = 0; i < result.length(); i++) {
                        Tuple2<String, Object> res = result.apply(i);
                        Object[] row = {new Integer(i + 1).toString(), res._1(), res._2(), false};
                        data[i] = row;
                    }
                }

                MyTableModel model = new MyTableModel(data);
                searchTable.setModel(model);

                resultTextArea.setText(
                        "Old Query: \n" +
                        searchEngine.oldQuery2Weights().toString() +
                        "\nNew Query: \n" +
                        searchEngine.newQuery2Weights().toString()
                );
            }
        });
        feedbackButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                switch (QTFComboBox.getSelectedIndex() ){
                    case 0 : tfKind = new coba.noTF(); System.out.println("No TF");break;
                    case 1 : tfKind = new coba.rawTF(); System.out.println("Raw TF");break;
                    case 2 : tfKind = new coba.logisticTF(); System.out.println("Log TF");break;
                    case 3 : tfKind = new coba.binaryTF(); System.out.println("Binary TF");break;
                    case 4 : tfKind = new coba.augmentedTF(); System.out.println("Augmented TF");break;
                }
                switch (feedbackSelection2.getSelectedIndex() ){
                    case 0 : feedKind = new coba.roccio(); System.out.println("Roccio");break;
                    case 1 : feedKind = new coba.ide_regular(); System.out.println("Ide Regular");break;
                    case 2 : feedKind = new coba.ide_dec_hi(); System.out.println("Ide Dec Hi");break;
                }

                //Relevance Feedback
                if (feedbackSelection1.getSelectedIndex() == 1) {
                    java.util.Set<String> relevances = new java.util.TreeSet<String>();
                    for (Object[] row : data) {
                        if (((Boolean)row[3]) == true)
                            relevances.add((String)row[1]);
                    }

                    Seq<Tuple2<String,Object>> result = expansionCheckBox.isSelected() ?
                            searchEngine.relevanceFeedbackSearchExpansion(
                                searchEngine.newQuery2Weights(),
                                Integer.parseInt(topNTextField.getText()),
                                feedKind,
                                coba.javaSet2scalaSet(relevances),
                                seachSeen.isSelected(),
                                coba.stripSimilarity(prev_result)) :
                            searchEngine.relevanceFeedbackSearch(
                                    searchEngine.newQuery2Weights(),
                                    Integer.parseInt(topNTextField.getText()),
                                    feedKind,
                                    coba.javaSet2scalaSet(relevances),
                                    seachSeen.isSelected(),
                                    coba.stripSimilarity(prev_result));

                    prev_result = result;

                    data = new Object[result.length()][];
                    for (int i = 0; i < result.length(); i++) {
                        Tuple2<String, Object> res = result.apply(i);
                        Object[] row = {new Integer(i + 1).toString(), res._1(), res._2(), false};
                        data[i] = row;
                    }
                }

                MyTableModel model = new MyTableModel(data);
                searchTable.setModel(model);

                resultTextArea.setText(
                        "Old Query: \n" +
                                searchEngine.oldQuery2Weights().toString() +
                                "\nNew Query: \n" +
                                searchEngine.newQuery2Weights().toString()
                );
            }
        });
    }

    public static void main(String[] args) {
        /* Use an appropriate Look and Feel */
        try {
            //UIManager.setLookAndFeel("com.sun.java.swing.plaf.windows.WindowsLookAndFeel");
            UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
        } catch (UnsupportedLookAndFeelException ex) {
            ex.printStackTrace();
        } catch (IllegalAccessException ex) {
            ex.printStackTrace();
        } catch (InstantiationException ex) {
            ex.printStackTrace();
        } catch (ClassNotFoundException ex) {
            ex.printStackTrace();
        }
        /* Turn off metal's use of bold fonts */
        UIManager.put("swing.boldMetal", Boolean.FALSE);

        //Schedule a job for the event dispatch thread:
        //creating and showing this application's GUI.
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                createAndShowGUI();
            }
        });
    }

    /**
     * Create the GUI and show it.  For thread safety,
     * this method should be invoked from the
     * event dispatch thread.
     */
    private static void createAndShowGUI() {
        //Create and set up the window.
        JFrame frame = new JFrame("CardLayoutDemo");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        //Create and set up the content pane.
        GUI gui = new GUI();
        frame.getContentPane().add(gui.cardPanel);

        //Display the window.
        frame.pack();
        frame.setVisible(true);
    }

    private void createUIComponents() {
        // TODO: place custom component creation code here
        MyTableModel model = new MyTableModel();
        searchTable = new JTable(model);
        searchTable.setPreferredScrollableViewportSize(searchTable.getPreferredSize());
    }

    class MyTableModel extends AbstractTableModel {
        private String[] columnNames = {"No", "Title", "Similarity", "Relevant?"};
        private Object[][] data = {
                {"1", "dummy", new Double(0), new Boolean(false)},
                {"1", "dummy", new Double(0), new Boolean(false)},
                {"1", "dummy", new Double(0), new Boolean(false)},
                {"1", "dummy", new Double(0), new Boolean(false)},
                {"1", "dummy", new Double(0), new Boolean(false)},
                {"1", "dummy", new Double(0), new Boolean(false)},
                {"1", "dummy", new Double(0), new Boolean(false)},
                {"1", "dummy", new Double(0), new Boolean(false)},
                {"1", "dummy", new Double(0), new Boolean(false)}
        };

        public MyTableModel() {

        }

        public MyTableModel(Object[][] data) {
            this.data = data;
        }

        public int getColumnCount() {
            return columnNames.length;
        }

        public int getRowCount() {
            return data.length;
        }

        public String getColumnName(int col) {
            return columnNames[col];
        }

        public Object getValueAt(int row, int col) {
            return data[row][col];
        }

        /*
         * JTable uses this method to determine the default renderer/
         * editor for each cell.  If we didn't implement this method,
         * then the last column would contain text ("true"/"false"),
         * rather than a check box.
         */
        public Class getColumnClass(int c) {
            return getValueAt(0, c).getClass();
        }

        /*
         * Don't need to implement this method unless your table's
         * editable.
         */
        public boolean isCellEditable(int row, int col) {
            //Note that the data/cell address is constant,
            //no matter where the cell appears onscreen.
            if (col == 3) {
                return true;
            } else {
                return false;
            }
        }

        /*
         * Don't need to implement this method unless your table's
         * data can change.
         */
        public void setValueAt(Object value, int row, int col) {
            data[row][col] = value;
            fireTableCellUpdated(row, col);
        }

        private void printDebugData() {
            int numRows = getRowCount();
            int numCols = getColumnCount();

            for (int i=0; i < numRows; i++) {
                System.out.print("    row " + i + ":");
                for (int j=0; j < numCols; j++) {
                    System.out.print("  " + data[i][j]);
                }
                System.out.println();
            }
            System.out.println("--------------------------");
        }
    }

}
