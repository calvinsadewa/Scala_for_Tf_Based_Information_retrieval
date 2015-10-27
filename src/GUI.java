import scala.collection.Seq;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

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
    private coba searchEngine = new coba();
    private coba.TF tfKind = new coba.noTF();

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

                resultTextArea.setText(searchEngine.experimentText(
                        tfKind,
                        QIDFCheckBox.isSelected(),
                        QnormalizationCheckBox.isSelected(),
                        QStemCheckBox.isSelected(),
                        queryLocationField.getText(),
                        relevanceJudgmentLocation.getText()));
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

                resultTextArea.setText(searchEngine.searchToText(
                        tfKind,
                        QIDFCheckBox.isSelected(),
                        QnormalizationCheckBox.isSelected(),
                        QStemCheckBox.isSelected(),
                        searchField.getText()));
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
}
