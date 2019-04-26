using Microsoft.EntityFrameworkCore;

namespace Grinder.DataAccess
{
    public class GrinderContext : DbContext
    {
        public DbSet<User> Users { get; set; }

        public static void MigrateUp()
        {
            using (var context = new GrinderContext())
            {
                context.Database.Migrate();
            }
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.Entity<User>()
                .HasIndex(b => b.Username);
            modelBuilder.Entity<User>()
                .HasIndex(b => b.UserId);
        }

        protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
        {
            optionsBuilder.UseSqlite("Data Source=data/grinder.db");
        }
    }
}