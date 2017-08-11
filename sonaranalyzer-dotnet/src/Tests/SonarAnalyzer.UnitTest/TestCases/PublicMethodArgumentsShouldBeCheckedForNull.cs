using System;

namespace Tests.Diagnostics
{
    public class Program
    {
        public void CompliantCases(bool b, object o1, object o2, object o3, object o4, Exception e)
        {
            if (o1 != null)
            {
                o1.ToString(); // Compliant, we did the check
            }
        }
    }
}
