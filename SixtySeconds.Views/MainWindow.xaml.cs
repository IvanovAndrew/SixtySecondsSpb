using System.Windows;

namespace SixtySeconds.Views
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void OnClosed(object sender, object args)
        {
            Properties.Settings.Default.Save();
        }
    }
}
